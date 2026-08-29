-- | Worker-side HTTP routing for Durable Object namespaces.
-- | `POST <prefix>/<class>/name/<id>/<method>` or `.../id/<hex>/<method>`
-- | sends an RPC request. `POST <prefix>/<class>/new` creates an id.
-- | WebSocket and object fetch paths are also forwarded to the object.
module Cloudflare.Durable.Http
  ( route
  ) where

import Prelude

import Cloudflare.Durable.Core (Namespace(..), className, idCodec, parseId, printId)
import Cloudflare.Worker (Route)
import Cloudflare.Worker as Worker
import Data.Array (drop, take)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split, stripPrefix)

-- | The Worker end: serve a namespace under `prefix`.
route :: forall name api events. String -> Namespace name api events -> Route
route prefix (Namespace { object, transport }) = Worker.route \request ->
  case Worker.method request, path request of
    "POST", Just [ klass, kind, value, methodName ] | klass == name, Just id <- parseId kind value -> do
      body <- Worker.body request
      envelope <- transport.call id methodName body
      pure $ Just $ Worker.json 200 envelope
    "GET", Just [ klass, kind, value, "socket" ]
      | klass == name, Just id <- parseId kind value, Worker.header request "upgrade" == Just "websocket" ->
          Just <$> transport.fetch id request
    _, Just segments
      | [ klass, kind, value, "http" ] <- take 4 segments, klass == name, Just id <- parseId kind value ->
          Just <$> transport.fetch id (Worker.rebase ("/" <> joinWith "/" (drop 4 segments)) request)
    "POST", Just [ klass, "new" ] | klass == name -> do
      id <- transport.unique
      pure $ Just $ Worker.json 200 $ CA.encode idCodec { id: printId id }
    _, _ -> pure Nothing
  where
  name = className object
  path request = split (Pattern "/") <$> stripPrefix (Pattern (prefix <> "/")) (Worker.pathname request)
