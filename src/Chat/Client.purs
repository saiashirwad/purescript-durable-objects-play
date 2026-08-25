-- | The browser's view of a room. `open` gives the same `Record RoomApi` the
-- | Worker holds, over HTTP; `feed` unfolds `since` into an `Emitter`.
module Chat.Client
  ( Chat
  , RoomId
  , connect
  , create
  , describeFailure
  , feed
  , open
  , parseRoomId
  , printRoomId
  ) where

import Prelude

import Chat.Room (Message, RoomApi, room)
import Cloudflare.Durable (Namespace, ObjectId)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Http as Http
import Cloudflare.Durable.Rpc (RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (last)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String (null, trim)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Halogen.Subscription (Emitter, makeEmitter)

newtype Chat = Chat (Namespace "Room" RoomApi)

type RoomId = ObjectId "Room"

connect :: String -> Chat
connect prefix = Chat $ Http.connect prefix room

create :: Chat -> Aff RoomId
create (Chat rooms) = Durable.newUniqueId rooms

open :: Chat -> RoomId -> Record RoomApi
open (Chat rooms) = Durable.get rooms

printRoomId :: RoomId -> String
printRoomId = Durable.idToString

parseRoomId :: Chat -> String -> Maybe RoomId
parseRoomId (Chat rooms) raw =
  let
    text = trim raw
  in
    if null text then Nothing else Just $ Durable.idFromString rooms text

-- | Every message with id above `after`, then each new one as it arrives.
-- | Polling starts on subscribe and stops on unsubscribe; a failed poll
-- | retries after two seconds. The anamorphism over the cursor is `tailRecM`.
feed :: Record RoomApi -> Int -> Emitter Message
feed r after = makeEmitter \push -> do
  fiber <- launchAff $ tailRecM (poll push) after
  pure $ launchAff_ $ killFiber (error "unsubscribed") fiber
  where
  poll push cursor = Rpc.run (r.since cursor) >>= case _ of
    Right messages -> do
      liftEffect $ traverse_ push messages
      pure $ Loop $ maybe cursor _.id $ last messages
    Left _ -> do
      delay $ Milliseconds 2000.0
      pure $ Loop cursor

describeFailure :: forall e. Show e => RpcFailure e -> String
describeFailure = case _ of
  DomainError e -> show e
  PlatformError e -> "The server could not do that: " <> show e
  TransportError message -> "Could not reach the server: " <> message
  DecodeError message -> "The server sent something unexpected: " <> message
  RemoteDefect message -> "The server failed: " <> message
