-- | `POST <prefix>/<class>/name/<id>/<method>` or `.../id/<hex>/<method>`,
-- | body the encoded request, response the envelope.
-- | `POST <prefix>/<class>/new` returns `{ "id": "<hex>" }`.
-- | No authentication: put `route` behind your own.
module Cloudflare.Durable.Http
  ( connect
  , route
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Namespace(..), Object, className, namespace)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Cloudflare.Worker (Route)
import Cloudflare.Worker as Worker
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix)
import Effect (Effect)
import Effect.Aff (error, throwError)

foreign import postJson :: String -> Json -> Effect (Promise Json)

route :: forall name api. String -> Namespace name api -> Route
route prefix (Namespace ns) = Worker.route \request ->
  case Worker.method request, path request of
    "POST", Just [ klass, kind, value, methodName ] | klass == ns.name, Just id <- decodeId kind value -> do
      body <- Worker.body request
      envelope <- ns.call id methodName body
      pure $ Just $ Worker.json 200 envelope
    "POST", Just [ klass, "new" ] | klass == ns.name -> do
      id <- ns.unique
      pure $ Just $ Worker.json 200 $ CA.encode idCodec { id: idToString' id }
    _, _ -> pure Nothing
  where
  path request = split (Pattern "/") <$> stripPrefix (Pattern (prefix <> "/")) (Worker.pathname request)

  decodeId "name" value = Just $ Named value
  decodeId "id" value = Just $ Unique value
  decodeId _ _ = Nothing

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
