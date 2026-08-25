-- | The chat from the client's side. A `Chat` is a connection to the server;
-- | a `Room` is one conversation, addressed by a `RoomId` that doubles as the
-- | invitation link. `listen` follows a room until you `stop`.
-- |
-- | ```purescript
-- | let chat = Chat.connect "/rpc"
-- | id <- Chat.createRoom chat
-- | let room = Chat.openRoom chat id
-- | subscription <- Chat.listen room 0 \messages -> log (show messages)
-- | _ <- Chat.send room { author: "ann", text: "hello" }
-- | Chat.stop subscription
-- | ```
module Chat.Client
  ( Chat
  , Room
  , RoomId
  , Subscription
  , connect
  , createRoom
  , describeFailure
  , history
  , listen
  , openRoom
  , parseRoomId
  , printRoomId
  , roomId
  , send
  , stop
  ) where

import Prelude

import Chat.Room (Message, NewMessage, PostError, RoomApi, room)
import Cloudflare.Durable (Namespace, ObjectId)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Http as Http
import Cloudflare.Durable.Rpc (NoError, RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Data.Array (last)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (null, trim)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, forkAff, killFiber, error)
import Effect.Class (liftEffect)

newtype Chat = Chat (Namespace "Room" RoomApi)

type RoomId = ObjectId "Room"

newtype Room = Room { id :: RoomId, stub :: Record RoomApi }

newtype Subscription = Subscription (Fiber Unit)

-- | Connect to a server that runs `Http.serve prefix rooms`.
connect :: String -> Chat
connect prefix = Chat $ Http.connect prefix room

-- | A fresh room with an id nobody can guess.
createRoom :: Chat -> Aff RoomId
createRoom (Chat rooms) = Durable.newUniqueId rooms

openRoom :: Chat -> RoomId -> Room
openRoom (Chat rooms) id = Room { id, stub: Durable.get rooms id }

roomId :: Room -> RoomId
roomId (Room r) = r.id

printRoomId :: RoomId -> String
printRoomId = Durable.idToString

-- | Read an id back from a link.
parseRoomId :: Chat -> String -> Maybe RoomId
parseRoomId (Chat rooms) raw =
  let
    text = trim raw
  in
    if null text then Nothing else Just $ Durable.idFromString rooms text

history :: Room -> Aff (Either (RpcFailure NoError) (Array Message))
history (Room r) = Rpc.run $ r.stub.history unit

send :: Room -> NewMessage -> Aff (Either (RpcFailure PostError) Message)
send (Room r) = Rpc.run <<< r.stub.post

-- | Follow a room from message id `after` (0 for everything). The callback
-- | gets each batch of new messages. A failed poll waits a moment and tries
-- | again, so a flaky connection does not end the subscription.
listen :: Room -> Int -> (Array Message -> Effect Unit) -> Aff Subscription
listen (Room r) after deliver = Subscription <$> forkAff (loop after)
  where
  loop cursor = do
    outcome <- Rpc.run $ r.stub.since cursor
    case outcome of
      Right messages -> do
        liftEffect $ deliver messages
        loop $ maybe cursor _.id (last messages)
      Left _ -> do
        delay $ Milliseconds 2000.0
        loop cursor

stop :: Subscription -> Aff Unit
stop (Subscription fiber) = killFiber (error "unsubscribed") fiber

-- | A short line for the screen.
describeFailure :: forall e. Show e => RpcFailure e -> String
describeFailure = case _ of
  DomainError e -> show e
  PlatformError e -> "The server could not do that: " <> show e
  TransportError message -> "Could not reach the server: " <> message
  DecodeError message -> "The server sent something unexpected: " <> message
  RemoteDefect message -> "The server failed: " <> message
