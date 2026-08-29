module Chat.Session
  ( RoomId
  , admitted
  , RoomSession
  , create
  , fromRoute
  , loginStatus
  , open
  , printRoomId
  , route
  , sessionStatus
  ) where

import Prelude

import Chat.Client as Client
import Data.Either (Either)
import Chat.Room (ImageId, Message, MessageId, NewMessage, RoomEvents, Snapshot, describePostError, describeReactError, printImageId)
import Cloudflare.Durable (Signal)
import Cloudflare.Durable.Rpc as Rpc
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Fetch (Method(..), fetch)
import Foreign.Object as Object
import Data.Argonaut.Core as J
import Halogen.Subscription (Emitter)

type RoomId = Client.RoomId

type RoomSession =
  { id :: RoomId
  , listen :: String -> Emitter (Signal (Variant RoomEvents))
  , post :: NewMessage -> Aff (Either String Message)
  , react :: { id :: MessageId, emoji :: String, by :: String } -> Aff (Either String Message)
  , snapshot :: Aff (Either String Snapshot)
  , typing :: String -> Aff Unit
  , imageEndpoint :: String
  , imageUrl :: ImageId -> String
  }

open :: RoomId -> RoomSession
open id =
  let
    api = Client.open Client.rpc id
    endpoint = "/rpc/Room/id/" <> Client.printRoomId id <> "/http/image"
  in
    { id
    , listen: Client.listen Client.rpc id
    , post: \message -> lmap (Client.describeFailure describePostError) <$> Rpc.run (api.post message)
    , react: \reaction -> lmap (Client.describeFailure describeReactError) <$> Rpc.run (api.react reaction)
    , snapshot: lmap (Client.describeFailure absurd) <$> Rpc.run (api.snapshot unit)
    , typing: \name -> void $ Rpc.run $ api.typing name
    , imageEndpoint: endpoint
    , imageUrl: \image -> endpoint <> "/" <> show (printImageId image)
    }

create :: Aff RoomId
create = Client.create Client.rpc

printRoomId :: RoomId -> String
printRoomId = Client.printRoomId

route :: RoomId -> String
route id = "#" <> printRoomId id

fromRoute :: String -> Maybe RoomId
fromRoute raw = Client.parseRoomId Client.rpc $ fromMaybe raw $ stripPrefix (Pattern "#") raw

sessionStatus :: Aff Int
sessionStatus = _.status <$> fetch "/session" {}

admitted :: Int -> Boolean
admitted status = status == 204

loginStatus :: String -> Aff Int
loginStatus passkey = _.status <$> fetch "/login"
  { method: POST
  , headers: { "content-type": "application/json" }
  , body: J.stringify $ J.fromObject $ Object.singleton "passkey" $ J.fromString passkey
  }
