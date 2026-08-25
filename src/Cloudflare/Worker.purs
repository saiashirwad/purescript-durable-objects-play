module Cloudflare.Worker
  ( ObjectBinding
  , Plan
  , Request
  , Response
  , Route
  , Worker
  , WorkerInit
  , WorkerRef
  , body
  , json
  , make
  , method
  , objectBinding
  , pathname
  , plan
  , ref
  , route
  , scriptName
  , serve
  , text
  , toExport
  , url
  , variable
  , wranglerConfig
  ) where

import Prelude

import Cloudflare.Static (Static, static)
import Cloudflare.Static as Static
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Plus (empty)
import Control.Apply (lift2)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Foreign.Object as Object

foreign import data Request :: Type
foreign import data Response :: Type
foreign import url :: Request -> String
foreign import method :: Request -> String
foreign import pathname :: Request -> String
foreign import text :: Int -> String -> Response
foreign import json :: Int -> Json -> Response
foreign import bodyImpl :: Request -> Effect (Promise Json)
foreign import variableImpl :: Foreign -> String -> Effect String
foreign import bindingImpl :: Foreign -> String -> Effect Foreign
foreign import toExportImpl :: (Foreign -> Effect { fetch :: Request -> Effect (Promise Response) }) -> Foreign

type ObjectBinding = { className :: String, binding :: String, scriptName :: Maybe String }

type Plan = { objects :: Array ObjectBinding, variables :: Array String }

type WorkerInit = Static Plan Foreign Effect

variable :: String -> WorkerInit String
variable name = static { objects: [], variables: [ name ] } \env -> variableImpl env name

objectBinding :: ObjectBinding -> WorkerInit Foreign
objectBinding b = static { objects: [ b ], variables: [] } \env -> bindingImpl env b.binding

body :: Request -> Aff Json
body = toAffE <<< bodyImpl

newtype WorkerRef = WorkerRef String

ref :: String -> WorkerRef
ref = WorkerRef

scriptName :: WorkerRef -> String
scriptName (WorkerRef name) = name

newtype Worker = Worker (WorkerInit Route)

-- | `a <> b` binds what both bind and tries `a`'s routes first.
instance semigroupWorker :: Semigroup Worker where
  append (Worker a) (Worker b) = Worker $ lift2 append a b

instance monoidWorker :: Monoid Worker where
  mempty = Worker $ pure mempty

make :: WorkerInit Route -> Worker
make = Worker

-- | `a <> b` tries `a`, then `b`. `mempty` matches nothing.
newtype Route = Route (Request -> MaybeT Aff Response)

instance semigroupRoute :: Semigroup Route where
  append (Route f) (Route g) = Route \request -> f request <|> g request

instance monoidRoute :: Monoid Route where
  mempty = Route \_ -> empty

route :: (Request -> Aff (Maybe Response)) -> Route
route handler = Route $ MaybeT <<< handler

-- | 404 when nothing matches.
serve :: Route -> Request -> Aff Response
serve (Route handler) request = runMaybeT (handler request) <#> case _ of
  Just response -> response
  Nothing -> text 404 "not found"

plan :: Worker -> Plan
plan (Worker w) = Static.plan w

toExport :: Worker -> Foreign
toExport (Worker w) = toExportImpl \env -> do
  routes <- Static.build w env
  pure { fetch: fromAff <<< serve routes }

wranglerConfig
  :: { name :: String, main :: String, compatibilityDate :: String, assets :: Maybe String }
  -> Worker
  -> Json
wranglerConfig options worker = J.fromObject $ Object.fromFoldable $
  [ "name" /\ J.fromString options.name
  , "main" /\ J.fromString options.main
  , "compatibility_date" /\ J.fromString options.compatibilityDate
  , "durable_objects" /\ J.fromObject (Object.singleton "bindings" (J.fromArray (bindingJson <$> objects)))
  , "exports" /\ J.fromObject (Object.fromFoldable (exportJson <$> hosted))
  ] <> case options.assets of
    Just directory -> [ "assets" /\ J.fromObject (Object.singleton "directory" (J.fromString directory)) ]
    Nothing -> []
  where
  objects = Array.nubEq (plan worker).objects
  hosted = objects # Array.filter (\o -> o.scriptName == Nothing)

  bindingJson o = J.fromObject $ Object.fromFoldable $
    [ "name" /\ J.fromString o.binding, "class_name" /\ J.fromString o.className ]
      <> case o.scriptName of
        Just script -> [ "script_name" /\ J.fromString script ]
        Nothing -> []

  exportJson o = o.className /\ J.fromObject
    (Object.fromFoldable [ "type" /\ J.fromString "durable-object", "storage" /\ J.fromString "sqlite" ])
