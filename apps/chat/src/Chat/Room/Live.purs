module Chat.Room.Live
  ( roomLive
  , roomLiveWith
  ) where

import Prelude

import Ai (Model)
import Ai.Catalogue as Catalogue
import Ai.Exa as Exa
import Ai.Provider as Provider
import Chat.Room (Message, NewMessage, PostError(..), ReactError(..), RoomApi, RoomEvents, Snapshot, UserNameError(..), maxTextLength, mkUserName, printUserName, room)
import Chat.Room.Assistant as Assistant
import Chat.Room.Images as Images
import Chat.Room.Store as Store
import Cloudflare.Durable (Handlers, Init, Live, Runtime)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Runtime (class MonadRuntime, liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Data.Array (null, nub)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (length, trim)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

-- The room -------------------------------------------------------------------

-- | Everything a handler needs, built once per activation.
type Room =
  { store :: Store.Store
  , images :: Images.Images
  , emit :: Channels
  , assistant :: Assistant.Assistant
  }

-- | The socket, narrowed to each event: `cmap` on a `Contravariant`.
type Channels =
  { all :: Sockets (Variant RoomEvents)
  , message :: Sockets Message
  , updated :: Sockets Message
  , presence :: Sockets (Array String)
  , typing :: Sockets String
  }

channels :: Sockets (Variant RoomEvents) -> Channels
channels all =
  { all
  , message: cmap (inj (Proxy :: Proxy "message")) all
  , updated: cmap (inj (Proxy :: Proxy "updated")) all
  , presence: cmap (inj (Proxy :: Proxy "presence")) all
  , typing: cmap (inj (Proxy :: Proxy "typing")) all
  }

roomLive :: Live "Room" RoomApi RoomEvents
roomLive = roomLiveWith \key -> Provider.model Provider.deepseek key Catalogue.deepseekFlash

-- | The room, given how to build a model from the API key. Tests pass a
-- | scripted one.
roomLiveWith :: (String -> Model Aff) -> Live "Room" RoomApi RoomEvents
roomLiveWith modelFor = Durable.implementWith room $ map handlersFor <$> open modelFor

-- | Plan what the room needs, then load it.
open :: (String -> Model Aff) -> Init (Runtime Room)
open modelFor = ado
  state <- Durable.state
  sockets <- Durable.sockets room
  apiKey <- Durable.optional "DEEPSEEK_API_KEY"
  exaKey <- Durable.optional "EXA_API_KEY"
  in
    do
      images <- Images.open state
      store <- Store.open state
      history <- Store.snapshot store
      attachedAt <- unwrap <<< unInstant <$> Alarm.now state
      for_ history \message -> Images.attach images attachedAt message.images
      let emit = channels sockets
      assistant <- Assistant.open
        { state
        , store
        , all: emit.all
        , message: emit.message
        , typing: emit.typing
        , model: modelFor <$> apiKey
        , search: Exa.search <$> exaKey
        }
      Images.cleanup images
      pure { store, images, emit, assistant }

handlersFor :: Room -> Handlers RoomApi
handlersFor r =
  Durable.handlers
    { post: post r
    , react: react r
    , snapshot: snapshot r
    , typing: Sockets.broadcast r.emit.typing
    }
    `Durable.withHooks`
      ( Assistant.hooks r.assistant
          <> Images.hooks r.images
          <> Durable.connectHook (const $ broadcastPresence r)
          <> Durable.disconnectHook (const $ broadcastPresence r)
      )

-- Messages ----------------------------------------------------------------------

members :: forall m. MonadRuntime m => Room -> m (Array String)
members r = nub <<< map _.tag <$> Sockets.connected r.emit.all

-- | Store one message and broadcast it after every write succeeds.
record :: Room -> NewMessage -> Runtime Message
record r new = do
  message <- Assistant.post r.assistant new
  Images.attach r.images message.sentAt new.images
  Sockets.broadcast r.emit.message message
  pure message

snapshot :: Room -> Unit -> Rpc Void Snapshot
snapshot r _ = do
  messages <- liftRuntime $ Store.snapshot r.store
  presence <- members r
  pure { messages, presence }

broadcastPresence :: Room -> Runtime Unit
broadcastPresence r = members r >>= Sockets.broadcast r.emit.presence

-- | Validate, record, and queue a reply if the assistant was mentioned.
post :: Room -> NewMessage -> Rpc PostError Message
post r new = do
  let body = trim new.text
  user <- case mkUserName new.author of
    Left UserNameRequired -> fail AuthorRequired
    Left UserNameTooLong -> fail AuthorTooLong
    Left UserNameInvalid -> fail AuthorInvalid
    Left UserNameReserved -> fail AuthorReserved
    Right accepted -> pure accepted
  let author = printUserName user
  when (body == "" && null new.images) $ fail TextRequired
  when (length body > maxTextLength) $ fail TextTooLong
  for_ new.replyTo \id -> do
    found <- liftRuntime $ Store.hasMessage r.store id
    unless found $ fail $ NoSuchReply id
  for_ new.images \id -> do
    found <- liftRuntime $ Images.exists r.images id
    unless found $ fail $ NoSuchImage id
  liftRuntime $ record r new { author = author, text = body }

react :: Room -> { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
react r { id, emoji, by } = do
  let normalizedEmoji = trim emoji
  when (normalizedEmoji == "") $ fail EmojiRequired
  reactor <- case mkUserName by of
    Left UserNameRequired -> fail ReactorRequired
    Left UserNameTooLong -> fail ReactorTooLong
    Left UserNameInvalid -> fail ReactorInvalid
    Left UserNameReserved -> fail ReactorReserved
    Right accepted -> pure accepted
  let normalizedReactor = printUserName reactor
  liftRuntime (Store.react r.store { id, emoji: normalizedEmoji, by: normalizedReactor }) >>= case _ of
    Nothing -> fail $ NoSuchMessage id
    Just message -> do
      liftRuntime $ Sockets.broadcast r.emit.updated message
      pure message

