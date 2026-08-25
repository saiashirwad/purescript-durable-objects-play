module Cloudflare.Durable.Platform
  ( namespaceFromBinding
  , stateFromContext
  , variablesFrom
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Namespace, Object, namespace)
import Cloudflare.Durable.Runtime (Listing, State(..))
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object as Object

foreign import storageGet :: Foreign -> String -> Effect (Promise (Nullable Json))
foreign import storagePut :: Foreign -> String -> Json -> Effect (Promise Unit)
foreign import storageDelete :: Foreign -> String -> Effect (Promise Boolean)
foreign import storageList :: Foreign -> { prefix :: String, limit :: Nullable Int, reverse :: Boolean } -> Effect (Promise (Array { key :: String, value :: Json }))
foreign import storageDeleteAll :: Foreign -> Effect (Promise Unit)
foreign import now :: Effect Number
foreign import alarmSet :: Foreign -> Number -> Effect (Promise Unit)
foreign import alarmGet :: Foreign -> Effect (Promise (Nullable Number))
foreign import alarmDelete :: Foreign -> Effect (Promise Unit)
foreign import sqlExec :: Foreign -> String -> Array Json -> Effect (Array Json)
foreign import variables :: Foreign -> Array String -> Effect (Object.Object String)
foreign import call :: Foreign -> { kind :: String, value :: String } -> String -> Json -> Effect (Promise Json)
foreign import unique :: Foreign -> Effect String

stateFromContext :: Foreign -> State
stateFromContext ctx = State
  { get: \key -> toMaybe <$> toAffE (storageGet ctx key)
  , put: \key value -> toAffE (storagePut ctx key value)
  , delete: \key -> toAffE (storageDelete ctx key)
  , list: \(options :: Listing) ->
      map (\{ key, value } -> Tuple key value)
        <$> toAffE (storageList ctx { prefix: options.prefix, limit: toNullable options.limit, reverse: options.reverse })
  , deleteAll: toAffE (storageDeleteAll ctx)
  , now: liftEffect now >>= toInstant
  , setAlarm: \at -> toAffE (alarmSet ctx (unwrap (unInstant at)))
  , getAlarm: toAffE (alarmGet ctx) >>= toMaybe >>> case _ of
      Just ms -> Just <$> toInstant ms
      Nothing -> pure Nothing
  , deleteAlarm: toAffE (alarmDelete ctx)
  , sql: \text bindings -> liftEffect (sqlExec ctx text bindings)
  }

toInstant :: Number -> Aff Instant
toInstant ms = case instant (wrap ms) of
  Just at -> pure at
  Nothing -> throwError $ error $ "time out of range: " <> show ms

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
