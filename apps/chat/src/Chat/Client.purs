-- | The browser's view of a room. `open` gives the same `Record RoomApi` the
-- | Worker holds, over HTTP; `listen` is the object's event socket as an
-- | `Emitter`.
module Chat.Client
  ( Chat
  , RoomId
  , connect
  , create
  , describeFailure
  , listen
  , open
  , parseRoomId
  , printRoomId
  ) where

import Prelude

import Chat.Room (RoomApi, RoomEvents, room)
import Cloudflare.Durable (Namespace, ObjectId, Signal)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Client as Client
import Cloudflare.Durable.Rpc (RpcFailure(..))
import Data.Maybe (Maybe(..))
import Data.String (null, trim)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Halogen.Subscription (Emitter, makeEmitter)

newtype Chat = Chat (Namespace "Room" RoomApi RoomEvents)

type RoomId = ObjectId "Room"

connect :: String -> Chat
connect prefix = Chat $ Client.connect prefix room

create :: Chat -> Aff RoomId
create (Chat rooms) = Durable.newUniqueId rooms

open :: Chat -> RoomId -> Record RoomApi
open (Chat rooms) = Durable.get rooms

-- | Connect as `tag`; the socket lives while the emitter is subscribed and
-- | reconnects on its own after a drop (`Opened`/`Closed` mark each).
listen :: Chat -> RoomId -> String -> Emitter (Signal (Variant RoomEvents))
listen (Chat rooms) id tag = makeEmitter $ Durable.listen rooms id tag

printRoomId :: RoomId -> String
printRoomId = Durable.idToString

parseRoomId :: Chat -> String -> Maybe RoomId
parseRoomId (Chat rooms) raw =
  let
    text = trim raw
  in
    if null text then Nothing else Just $ Durable.idFromString rooms text

describeFailure :: forall e. Show e => RpcFailure e -> String
describeFailure = case _ of
  DomainError e -> show e
  PlatformError e -> "The server could not do that: " <> show e
  TransportError message -> "Could not reach the server: " <> message
  DecodeError message -> "The server sent something unexpected: " <> message
  RemoteDefect message -> "The server failed: " <> message
