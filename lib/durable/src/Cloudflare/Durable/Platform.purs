module Cloudflare.Durable.Platform
  ( containerFromContext
  , namespaceFromBinding
  , socketsFromContext
  , stateFromContext
  , variablesFrom
  ) where

import Prelude

import Cloudflare.Durable.Container as Container
import Cloudflare.Durable.Core (Id(..), Namespace, Object, namespace)
import Cloudflare.Durable.Runtime (Exit(..), Launch, Listing, RawContainer, RawSockets, Socket, State(..))
import Cloudflare.Worker (Request, Response)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
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
foreign import sqlBatch :: Foreign -> Array { sql :: String, bindings :: Array Json } -> Effect (Array (Array Json))
foreign import variables :: Foreign -> Array String -> Effect (Object.Object String)
foreign import call :: Foreign -> { kind :: String, value :: String } -> String -> Json -> Effect (Promise Json)
foreign import unique :: Foreign -> Effect String
foreign import fetchObject :: Foreign -> { kind :: String, value :: String } -> Request -> Effect (Promise Response)
foreign import containerRunning :: Foreign -> Effect Boolean
foreign import containerStart :: Foreign -> { env :: Object.Object String, entrypoint :: Nullable (Array String), enableInternet :: Boolean } -> Effect Unit
foreign import containerProbe :: Foreign -> Int -> Effect (Promise Boolean)
foreign import containerRequest :: Foreign -> Int -> Request -> Effect (Promise Response)
foreign import containerSignal :: Foreign -> Int -> Effect Unit
foreign import containerDestroy :: Foreign -> Effect (Promise Unit)
foreign import containerExit :: Foreign -> Effect (Promise { code :: Nullable Int, lost :: Nullable String })

containerFromContext :: Foreign -> RawContainer
containerFromContext ctx =
  { running: liftEffect $ containerRunning ctx
  , start: \(launch :: Launch) -> liftEffect $ containerStart ctx
      { env: Object.fromFoldableWithIndex (Container.environment launch)
      , entrypoint: toNullable $ Container.command launch
      , enableInternet: Container.internet launch
      }
  , probe: \port -> toAffE $ containerProbe ctx port
  , request: \port req -> toAffE $ containerRequest ctx port req
  , signal: \code -> liftEffect $ containerSignal ctx code
  , destroy: toAffE $ containerDestroy ctx
  , exit: toAffE (containerExit ctx) <#> \{ code, lost } -> case toMaybe code, toMaybe lost of
      Just c, _ -> Exited c
      _, Just why -> Lost why
      _, _ -> Exited 0
  }

foreign import socketsBroadcast :: Foreign -> Json -> Effect Unit
foreign import socketsSend :: Foreign -> String -> Json -> Effect Unit
foreign import socketsConnected :: Foreign -> Effect (Array Socket)

socketsFromContext :: Foreign -> RawSockets
socketsFromContext ctx =
  { broadcast: liftEffect <<< socketsBroadcast ctx
  , send: \socket json -> liftEffect $ socketsSend ctx socket.id json
  , connected: liftEffect $ socketsConnected ctx
  }

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
  , getAlarm: toAffE (alarmGet ctx) >>= traverse toInstant <<< toMaybe
  , deleteAlarm: toAffE (alarmDelete ctx)
  , sql: \text bindings -> liftEffect (sqlExec ctx text bindings)
  , sqlBatch: liftEffect <<< sqlBatch ctx
  }

toInstant :: Number -> Aff Instant
toInstant ms = case instant (wrap ms) of
  Just at -> pure at
  Nothing -> throwError $ error $ "time out of range: " <> show ms

variablesFrom :: Foreign -> Array String -> Effect (Map String String)
variablesFrom env names = Map.fromFoldableWithIndex <$> variables env names

namespaceFromBinding :: forall name api events. Object name api events -> Foreign -> Namespace name api events
namespaceFromBinding object ns = namespace object
  { call: \id method request -> toAffE $ call ns (encodeId id) method request
  , unique: liftEffect $ Unique <$> unique ns
  , listen: \_ _ _ -> throwException $ error "listen is for browsers and the simulator; a Worker routes the upgrade with Http.route"
  , fetch: \id request -> toAffE $ fetchObject ns (encodeId id) request
  }
  where
  encodeId = case _ of
    Named name -> { kind: "named", value: name }
    Unique id -> { kind: "unique", value: id }
