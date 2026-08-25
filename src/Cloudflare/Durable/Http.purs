-- | A namespace over HTTP. A Worker runs `serve` to expose a namespace; a
-- | browser (or another service) runs `connect` to get a `Namespace` with the
-- | same `Record api` type the Worker uses. Calls carry the same JSON
-- | envelope as Durable Object RPC.
-- |
-- | Route shape: `POST <prefix>/<class>/name/<id>/<method>`, or `.../id/<hex>/...`
-- | for unique ids. The body is the encoded request; the response is the
-- | envelope. `POST <prefix>/<class>/new` mints a unique id and returns
-- | `{ "id": "<hex>" }`. `serve` exposes every method of the namespace: put
-- | it behind your own authentication.
module Cloudflare.Durable.Http
  ( connect
  , serve
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Namespace(..), Object, className, namespace)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Cloudflare.Worker (Request, Response)
import Cloudflare.Worker as Worker
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)

foreign import postJson :: String -> Json -> Effect (Promise Json)

-- | Answer requests under `prefix` for this namespace. `Nothing` means the
-- | request is not for it.
serve :: forall name api. String -> Namespace name api -> Request -> Aff (Maybe Response)
serve prefix (Namespace ns) request =
  case Worker.method request, route of
    "POST", Just [ klass, kind, value, methodName ] | klass == ns.name, Just id <- decodeId kind value -> do
      body <- Worker.body request
      envelope <- ns.call id methodName body
      pure $ Just $ Worker.json 200 envelope
    "POST", Just [ klass, "new" ] | klass == ns.name -> do
      id <- ns.unique
      pure $ Just $ Worker.json 200 $ CA.encode idCodec { id: idToString' id }
    _, _ -> pure Nothing
  where
  route = split (Pattern "/") <$> stripPrefix (Pattern (prefix <> "/")) (Worker.pathname request)

  decodeId "name" value = Just $ Named value
  decodeId "id" value = Just $ Unique value
  decodeId _ _ = Nothing

-- | A namespace whose calls go to `prefix` on a server that runs `serve`.
connect :: forall name api. String -> Object name api -> Namespace name api
connect prefix object = namespace object
  { call: \id methodName body -> toAffE $ postJson (url id methodName) body
  , unique: do
      response <- toAffE $ postJson (base <> "/new") J.jsonNull
      case CA.decode idCodec response of
        Right { id } -> pure $ Unique id
        Left err -> throwError $ error $ "Cloudflare.Durable.Http: bad id response: " <> CA.printJsonDecodeError err
  }
  where
  base = prefix <> "/" <> className object
  url id methodName = base <> "/" <> encodeId id <> "/" <> methodName

  encodeId = case _ of
    Named name -> "name/" <> name
    Unique id -> "id/" <> id

idCodec :: JsonCodec { id :: String }
idCodec = CAR.object "Id" { id: CA.string }

idToString' :: Id -> String
idToString' = case _ of
  Unique hex -> hex
  Named name -> name
