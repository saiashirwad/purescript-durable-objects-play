module Chat.Room.Live
  ( roomLive
  , roomLiveWith
  ) where

import Prelude

import Ai (Model)
import Ai.Catalogue as Catalogue
import Ai.Exa as Exa
import Ai.Provider as Provider
import Chat.Room (Message, RoomApi, RoomEvents, Snapshot, room)
import Chat.Room.Assistant as Assistant
import Chat.Room.Images as Images
import Chat.Room.Messages as Messages
import Chat.Room.Presence as Presence
import Chat.Room.Store as Store
import Cloudflare.Durable (Handlers, Init, Live, Runtime)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (Rpc)
import Cloudflare.Durable.Runtime (liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Functor.Contravariant (cmap)
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

-- The room -------------------------------------------------------------------

-- | Everything a handler needs, built once per activation.
type Room =
  { store :: Store.Store
  , images :: Images.Images
  , assistant :: Assistant.Assistant
  , messages :: Messages.Messages
  , presence :: Presence.Presence
  , typing :: Sockets String
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
      let
        messages = Messages.open
          { store
          , images
          , assistant
          , message: emit.message
          , updated: emit.updated
          }
        presence = Presence.open emit.all emit.presence
      Images.cleanup images
      pure { store, images, assistant, messages, presence, typing: emit.typing }

handlersFor :: Room -> Handlers RoomApi
handlersFor r =
  Durable.handlers
    { post: Messages.post r.messages
    , react: Messages.react r.messages
    , snapshot: snapshot r
    , typing: Sockets.broadcast r.typing
    }
    `Durable.withHooks`
      ( Assistant.hooks r.assistant
          <> Images.hooks r.images
          <> Presence.hooks r.presence
      )

snapshot :: Room -> Unit -> Rpc Void Snapshot
snapshot r _ = do
  messages <- liftRuntime $ Store.snapshot r.store
  presence <- Presence.members r.presence
  pure { messages, presence }
