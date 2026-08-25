module Cloudflare.Durable.Platform
  ( namespaceFromBinding
  , stateFromContext
  , variablesFrom
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Namespace, Object, namespace)
import Cloudflare.Durable.Runtime (State(..))
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Map (Map)
import Data.Map as Map
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object as Object

foreign import storageGet :: Foreign -> String -> Effect (Promise (Nullable Json))
foreign import storagePut :: Foreign -> String -> Json -> Effect (Promise Unit)
foreign import storageDelete :: Foreign -> String -> Effect (Promise Boolean)
foreign import variables :: Foreign -> Array String -> Effect (Object.Object String)
foreign import call :: Foreign -> { kind :: String, value :: String } -> String -> Json -> Effect (Promise Json)
foreign import unique :: Foreign -> Effect String

stateFromContext :: Foreign -> State
stateFromContext ctx = State
  { get: \key -> toMaybe <$> toAffE (storageGet ctx key)
  , put: \key value -> toAffE (storagePut ctx key value)
  , delete: \key -> toAffE (storageDelete ctx key)
  }

variablesFrom :: Foreign -> Array String -> Effect (Map String String)
variablesFrom env names = Map.fromFoldable <<< (Object.toUnfoldable :: _ -> Array _) <$> variables env names

namespaceFromBinding :: forall name api. Object name api -> Foreign -> Namespace name api
namespaceFromBinding object ns = namespace object
  { call: \id method request -> toAffE $ call ns (encodeId id) method request
  , unique: liftEffect $ Unique <$> unique ns
  }
  where
  encodeId = case _ of
    Named name -> { kind: "named", value: name }
    Unique id -> { kind: "unique", value: id }
