module Chat.Room.Messages
  ( Messages
  , open
  , post
  , react
  ) where

import Prelude

import Chat.Room (AcceptedMessage, Author(..), Message, MessageId, NewMessage, PostError(..), ReactError(..), UserNameError(..), maxTextLength, mkUserName, printUserName)
import Chat.Room.Assistant as Assistant
import Chat.Room.Images as Images
import Chat.Room.Store as Store
import Cloudflare.Durable (Runtime)
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Runtime (liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (length, trim)

type Config =
  { store :: Store.Store
  , images :: Images.Images
  , assistant :: Assistant.Assistant
  , message :: Sockets Message
  , updated :: Sockets Message
  }

newtype Messages = Messages Config

open :: Config -> Messages
open = Messages

record :: Messages -> AcceptedMessage -> Runtime Message
record (Messages messages) new = do
  message <- Assistant.post messages.assistant new
  Images.attach messages.images message.sentAt new.images
  Sockets.broadcast messages.message message
  pure message

post :: Messages -> NewMessage -> Rpc PostError Message
post messages@(Messages config) new = do
  let body = trim new.text
  user <- case mkUserName new.author of
    Left UserNameRequired -> fail AuthorRequired
    Left UserNameTooLong -> fail AuthorTooLong
    Left UserNameInvalid -> fail AuthorInvalid
    Left UserNameReserved -> fail AuthorReserved
    Right accepted -> pure accepted
  when (body == "" && null new.images) $ fail TextRequired
  when (length body > maxTextLength) $ fail TextTooLong
  for_ new.replyTo \id -> do
    found <- liftRuntime $ Store.hasMessage config.store id
    unless found $ fail $ NoSuchReply id
  for_ new.images \id -> do
    found <- liftRuntime $ Images.exists config.images id
    unless found $ fail $ NoSuchImage id
  liftRuntime $ record messages { author: Human user, text: body, images: new.images, replyTo: new.replyTo }

react :: Messages -> { id :: MessageId, emoji :: String, by :: String } -> Rpc ReactError Message
react (Messages messages) { id, emoji, by } = do
  let normalizedEmoji = trim emoji
  when (normalizedEmoji == "") $ fail EmojiRequired
  reactor <- case mkUserName by of
    Left UserNameRequired -> fail ReactorRequired
    Left UserNameTooLong -> fail ReactorTooLong
    Left UserNameInvalid -> fail ReactorInvalid
    Left UserNameReserved -> fail ReactorReserved
    Right accepted -> pure accepted
  let normalizedReactor = printUserName reactor
  liftRuntime (Store.react messages.store { id, emoji: normalizedEmoji, by: normalizedReactor }) >>= case _ of
    Nothing -> fail $ NoSuchMessage id
    Just message -> do
      liftRuntime $ Sockets.broadcast messages.updated message
      pure message
