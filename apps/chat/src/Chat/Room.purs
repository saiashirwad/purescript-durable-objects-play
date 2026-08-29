module Chat.Room
  ( Message
  , NewMessage
  , PostError(..)
  , ReactError(..)
  , Reaction
  , RoomApi
  , RoomEvents
  , module Domain
  , describePostError
  , describeReactError
  , maxTextLength
  , room
  ) where

import Prelude

import Cloudflare.Durable (Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Codec (class HasCodec)
import Chat.Room.Domain (UserNameError(..), describeUserNameError)
import Chat.Room.Domain (UserName, UserNameError(..), assistantName, describeUserNameError, maxUserNameLength, mkUserName, printUserName) as Domain
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

type Reaction = { emoji :: String, by :: Array String }

-- | `text` is Markdown. `images` are ids served by the room at `/image/<id>`.
-- | `mentions` are the `@names` the server found in the text.
type Message =
  { id :: Int
  , author :: String
  , text :: String
  , images :: Array Int
  , replyTo :: Maybe Int
  , mentions :: Array String
  , reactions :: Array Reaction
  , sentAt :: Number
  }

type NewMessage = { author :: String, text :: String, images :: Array Int, replyTo :: Maybe Int }

data PostError
  = AuthorRequired
  | TextRequired
  | AuthorTooLong
  | AuthorInvalid
  | AuthorReserved
  | TextTooLong
  | NoSuchReply Int
  | NoSuchImage Int

derive instance eqPostError :: Eq PostError

instance showPostError :: Show PostError where
  show = case _ of
    AuthorRequired -> "AuthorRequired"
    TextRequired -> "TextRequired"
    AuthorTooLong -> "AuthorTooLong"
    AuthorInvalid -> "AuthorInvalid"
    AuthorReserved -> "AuthorReserved"
    TextTooLong -> "TextTooLong"
    NoSuchReply id -> "(NoSuchReply " <> show id <> ")"
    NoSuchImage id -> "(NoSuchImage " <> show id <> ")"

describePostError :: PostError -> String
describePostError = case _ of
  AuthorRequired -> "Enter your name."
  AuthorTooLong -> describeUserNameError UserNameTooLong
  AuthorInvalid -> describeUserNameError UserNameInvalid
  AuthorReserved -> describeUserNameError UserNameReserved
  TextRequired -> "Write a message or attach an image."
  TextTooLong -> "The message is too long."
  NoSuchReply _ -> "The message you replied to is not available."
  NoSuchImage _ -> "An attached image is not available."

-- | `{ tag, id }` on the wire; a sum with at most one `Int` payload needs no
-- | more. `prismaticCodec` is the codec of a partial isomorphism: every
-- | value prints, only well-formed wire values read.
instance hasCodecPostError :: HasCodec PostError where
  codec = tagged "PostError" print read
    where
    print = case _ of
      AuthorRequired -> "AuthorRequired" /\ Nothing
      AuthorTooLong -> "AuthorTooLong" /\ Nothing
      AuthorInvalid -> "AuthorInvalid" /\ Nothing
      AuthorReserved -> "AuthorReserved" /\ Nothing
      TextRequired -> "TextRequired" /\ Nothing
      TextTooLong -> "TextTooLong" /\ Nothing
      NoSuchReply id -> "NoSuchReply" /\ Just id
      NoSuchImage id -> "NoSuchImage" /\ Just id
    read = case _ of
      "AuthorRequired" /\ Nothing -> Just AuthorRequired
      "TextRequired" /\ Nothing -> Just TextRequired
      "AuthorTooLong" /\ Nothing -> Just AuthorTooLong
      "AuthorInvalid" /\ Nothing -> Just AuthorInvalid
      "AuthorReserved" /\ Nothing -> Just AuthorReserved
      "TextTooLong" /\ Nothing -> Just TextTooLong
      "NoSuchReply" /\ Just id -> Just (NoSuchReply id)
      "NoSuchImage" /\ Just id -> Just (NoSuchImage id)
      _ -> Nothing

data ReactError = NoSuchMessage Int | EmojiRequired | ReactorRequired | ReactorTooLong | ReactorInvalid | ReactorReserved

derive instance eqReactError :: Eq ReactError

instance showReactError :: Show ReactError where
  show = case _ of
    NoSuchMessage id -> "(NoSuchMessage " <> show id <> ")"
    EmojiRequired -> "EmojiRequired"
    ReactorRequired -> "ReactorRequired"
    ReactorTooLong -> "ReactorTooLong"
    ReactorInvalid -> "ReactorInvalid"
    ReactorReserved -> "ReactorReserved"

describeReactError :: ReactError -> String
describeReactError = case _ of
  NoSuchMessage _ -> "That message is not available."
  EmojiRequired -> "Select a reaction."
  ReactorRequired -> "Enter your name."
  ReactorTooLong -> describeUserNameError UserNameTooLong
  ReactorInvalid -> describeUserNameError UserNameInvalid
  ReactorReserved -> describeUserNameError UserNameReserved

instance hasCodecReactError :: HasCodec ReactError where
  codec = tagged "ReactError" print read
    where
    print = case _ of
      NoSuchMessage id -> "NoSuchMessage" /\ Just id
      EmojiRequired -> "EmojiRequired" /\ Nothing
      ReactorRequired -> "ReactorRequired" /\ Nothing
      ReactorTooLong -> "ReactorTooLong" /\ Nothing
      ReactorInvalid -> "ReactorInvalid" /\ Nothing
      ReactorReserved -> "ReactorReserved" /\ Nothing
    read = case _ of
      "NoSuchMessage" /\ Just id -> Just (NoSuchMessage id)
      "EmojiRequired" /\ Nothing -> Just EmojiRequired
      "ReactorRequired" /\ Nothing -> Just ReactorRequired
      "ReactorTooLong" /\ Nothing -> Just ReactorTooLong
      "ReactorInvalid" /\ Nothing -> Just ReactorInvalid
      "ReactorReserved" /\ Nothing -> Just ReactorReserved
      _ -> Nothing

tagged :: forall a. String -> (a -> Tuple String (Maybe Int)) -> (Tuple String (Maybe Int) -> Maybe a) -> JsonCodec a
tagged name print read =
  CA.prismaticCodec name (read <<< toPair) (fromPair <<< print) $ CAR.object name { tag: CA.string, id: CAR.optional CA.int }
  where
  toPair { tag, id } = tag /\ id
  fromPair (tag /\ id) = { tag, id }

maxTextLength :: Int
maxTextLength = 4000

type RoomApi =
  ( post :: NewMessage -> Rpc PostError Message
  , react :: { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
  , history :: Unit -> Rpc NoError (Array Message)
  , members :: Unit -> Rpc NoError (Array String)
  , typing :: String -> Rpc NoError Unit
  )

-- | Pushed to every open socket. `updated` carries a message whose
-- | reactions changed; `joined`, `left` and `typing` carry a name.
type RoomEvents =
  ( message :: Message
  , updated :: Message
  , joined :: String
  , left :: String
  , typing :: String
  )

room :: Object "Room" RoomApi RoomEvents
room =
  Durable.object { post: method, react: method, history: method, members: method, typing: method }
    `Durable.emitting`
      { message: Durable.event, updated: Durable.event, joined: Durable.event, left: Durable.event, typing: Durable.event }
