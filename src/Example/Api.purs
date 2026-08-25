module Example.Api
  ( api
  , chat
  , counter
  , echo
  ) where

import Prelude

import Chat.Room.Live (roomLive)
import Cloudflare.Durable (Namespace)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Http as Http
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Worker (Route, Worker)
import Cloudflare.Worker as Worker
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Example.Counter (CounterApi, counterLive)
import Example.Echo (echoLive)

api :: Worker
api = chat <> counter <> echo

chat :: Worker
chat = Worker.make ado
  rooms <- Durable.host roomLive
  in Http.route "/rpc" rooms

-- | `/rpc/Echo/name/<n>/http/<path>` proxies to that name's container.
echo :: Worker
echo = Worker.make ado
  echoes <- Durable.host echoLive
  in Http.route "/rpc" echoes

counter :: Worker
counter = Worker.make ado
  counters <- Durable.host counterLive
  in Http.route "/rpc" counters <> counterValue counters

-- | `GET /counter/:name`, the Worker calling the object itself.
counterValue :: Namespace "Counter" CounterApi () -> Route
counterValue counters = Worker.route \request ->
  case Worker.method request, split (Pattern "/") (Worker.pathname request) of
    "GET", [ "", "counter", name ] ->
      Rpc.run ((Durable.getByName counters name).get unit) <#> Just <<< case _ of
        Right value -> Worker.text 200 $ show value
        Left failure -> Worker.text 500 $ show failure
    _, _ -> pure Nothing
