-- | The Worker. It hosts the Counter and the chat Room, serves both to the
-- | browser under `/rpc`, and keeps one plain route that calls an object
-- | from the Worker itself:
-- |
-- | ```text
-- | POST /rpc/Room/new                       -> { id }      (a fresh room; the id is the link)
-- | POST /rpc/Room/id/:id/post               -> envelope
-- | POST /rpc/Counter/name/:name/increment   -> envelope
-- | GET  /counter/:name                      -> current value
-- | ```
module Example.Api
  ( api
  ) where

import Prelude

import Chat.Room.Live (roomLive)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Http as Http
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Worker (Request, Response, Worker)
import Cloudflare.Worker as Worker
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect.Aff (Aff)
import Example.Counter (counterLive)

api :: Worker
api = Worker.make ado
  counters <- Durable.host counterLive
  rooms <- Durable.host roomLive
  in
    { fetch: \request ->
        firstOf request
          [ Http.serve "/rpc" rooms
          , Http.serve "/rpc" counters
          , \req -> case Worker.method req, split (Pattern "/") (Worker.pathname req) of
              "GET", [ "", "counter", name ] ->
                Rpc.run ((Durable.getByName counters name).get unit) <#> Just <<< case _ of
                  Right value -> Worker.text 200 $ show value
                  Left failure -> Worker.text 500 $ show failure
              _, _ -> pure Nothing
          ]
    }

firstOf :: Request -> Array (Request -> Aff (Maybe Response)) -> Aff Response
firstOf request handlers = case uncons handlers of
  Just { head, tail } -> head request >>= case _ of
    Just response -> pure response
    Nothing -> firstOf request tail
  Nothing -> pure $ Worker.text 404 "not found"
