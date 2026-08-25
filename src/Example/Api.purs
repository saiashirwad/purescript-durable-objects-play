module Example.Api
  ( api
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

api :: Worker
api = Worker.make ado
  counters <- Durable.host counterLive
  rooms <- Durable.host roomLive
  in
    { fetch: Worker.serve $
        Http.route "/rpc" rooms
          <> Http.route "/rpc" counters
          <> counterValue counters
    }

counterValue :: Namespace "Counter" CounterApi -> Route
counterValue counters = Worker.route \request ->
  case Worker.method request, split (Pattern "/") (Worker.pathname request) of
    "GET", [ "", "counter", name ] ->
      Rpc.run ((Durable.getByName counters name).get unit) <#> Just <<< case _ of
        Right value -> Worker.text 200 $ show value
        Left failure -> Worker.text 500 $ show failure
    _, _ -> pure Nothing
