-- | `POST <prefix>/<class>/name/<id>/<method>` or `.../id/<hex>/<method>`,
-- | body the encoded request, response the envelope.
-- | `POST <prefix>/<class>/new` returns `{ "id": "<hex>" }`.
-- | `GET <prefix>/<class>/id/<hex>/socket?tag=<tag>` upgrades to a WebSocket
-- | that receives the object's events.
-- | `<prefix>/<class>/id/<hex>/http/<path>` reaches the object's `fetch` hook
-- | as `/<path>`; a container proxies it.
-- | No authentication: put `route` behind your own.
module Cloudflare.Durable.Http
  ( connect
  , route
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Listener, Namespace(..), Object, className, namespace, printId)
import Cloudflare.Durable.Events (Signal(..))
import Cloudflare.Worker (Route)
import Cloudflare.Worker as Worker
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array (drop, take)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split, stripPrefix)
import Effect (Effect)
import Effect.Aff (error, throwError)

foreign import postJson :: String -> Json -> Effect (Promise Json)
foreign import openSocket :: String -> Effect Unit -> Effect Unit -> (Json -> Effect Unit) -> (String -> Effect Unit) -> Effect (Effect Unit)

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

-- | The browser end: the same namespace, reached over HTTP under `prefix`.
connect :: forall name api events. String -> Object name api events -> Namespace name api events
connect prefix object = namespace object
  { call: \id methodName body -> toAffE $ postJson (url id methodName) body
  , listen: \id tag (deliver :: Listener) ->
      openSocket (url id "socket?tag=" <> tag) (deliver Opened) (deliver Closed) (deliver <<< Delivered) (deliver <<< Garbled)
  , fetch: \_ _ -> throwError $ error "fetch into an object is for Workers; a browser goes through Http.route"
  , unique: do
      response <- toAffE $ postJson (base <> "/new") J.jsonNull
      case CA.decode idCodec response of
        Right { id } -> pure $ Unique id
        Left err -> throwError $ error $ "Cloudflare.Durable.Http: bad id response: " <> CA.printJsonDecodeError err
  }
  where
  base = prefix <> "/" <> className object
  url id methodName = base <> "/" <> idSegments id <> "/" <> methodName

-- | `name/<n>` or `id/<hex>`; `parseId` reads it back.
idSegments :: Id -> String
idSegments = case _ of
  Named name -> "name/" <> name
  Unique id -> "id/" <> id

parseId :: String -> String -> Maybe Id
parseId "name" value = Just $ Named value
parseId "id" value = Just $ Unique value
parseId _ _ = Nothing

idCodec :: JsonCodec { id :: String }
idCodec = CAR.object "Id" { id: CA.string }
