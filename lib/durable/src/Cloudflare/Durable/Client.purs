-- | The browser end of the Durable Object transport.
module Cloudflare.Durable.Client
  ( connect
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Listener, Namespace, Object, className, namespace)
import Cloudflare.Durable.Events (Signal(..))
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (error, throwError)

foreign import postJson :: String -> Json -> Effect (Promise Json)

foreign import openSocket :: String -> Effect Unit -> Effect Unit -> (Json -> Effect Unit) -> (String -> Effect Unit) -> Effect (Effect Unit)

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
        Left err -> throwError $ error $ "Cloudflare.Durable.Client: bad id response: " <> CA.printJsonDecodeError err
  }
  where
  base = prefix <> "/" <> className object
  url id methodName = base <> "/" <> idSegments id <> "/" <> methodName

idSegments :: Id -> String
idSegments = case _ of
  Named name -> "name/" <> name
  Unique id -> "id/" <> id

idCodec :: JsonCodec { id :: String }
idCodec = CAR.object "Id" { id: CA.string }
