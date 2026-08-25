-- | A Worker as a value. `WorkerInit` is applicative like `Init`: its plan
-- | (bindings, variables) is visible without an `env`, so `wranglerConfig`
-- | can write the deployment from it.
module Cloudflare.Worker
  ( Handlers
  , ObjectBinding
  , Plan
  , Request
  , Response
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
  , scriptName
  , text
  , toExport
  , url
  , variable
  , wranglerConfig
  ) where

import Prelude

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

-- | A Durable Object namespace this Worker binds. `scriptName` is set when
-- | another Worker hosts the class.
type ObjectBinding = { className :: String, binding :: String, scriptName :: Maybe String }

type Plan = { objects :: Array ObjectBinding, variables :: Array String }

newtype WorkerInit a = WorkerInit { plan :: Plan, build :: Foreign -> Effect a }

instance functorWorkerInit :: Functor WorkerInit where
  map f (WorkerInit i) = WorkerInit { plan: i.plan, build: map f <<< i.build }

instance applyWorkerInit :: Apply WorkerInit where
  apply (WorkerInit f) (WorkerInit x) = WorkerInit
    { plan:
        { objects: f.plan.objects <> x.plan.objects
        , variables: f.plan.variables <> x.plan.variables
        }
    , build: \env -> f.build env <*> x.build env
    }

instance applicativeWorkerInit :: Applicative WorkerInit where
  pure a = WorkerInit { plan: { objects: [], variables: [] }, build: \_ -> pure a }

-- | A string variable from the Worker's environment.
variable :: String -> WorkerInit String
variable name = WorkerInit
  { plan: { objects: [], variables: [ name ] }
  , build: \env -> variableImpl env name
  }

-- | The raw namespace binding for an object. `Cloudflare.Durable.host` and
-- | `from` wrap this with a typed `Namespace`.
objectBinding :: ObjectBinding -> WorkerInit Foreign
objectBinding b = WorkerInit
  { plan: { objects: [ b ], variables: [] }
  , build: \env -> bindingImpl env b.binding
  }

-- | The request body, parsed as JSON.
body :: Request -> Aff Json
body = toAffE <<< bodyImpl

-- | Another Worker, by script name.
newtype WorkerRef = WorkerRef String

ref :: String -> WorkerRef
ref = WorkerRef

scriptName :: WorkerRef -> String
scriptName (WorkerRef name) = name

type Handlers = { fetch :: Request -> Aff Response }

newtype Worker = Worker { plan :: Plan, build :: Foreign -> Effect Handlers }

make :: WorkerInit Handlers -> Worker
make (WorkerInit i) = Worker { plan: i.plan, build: i.build }

plan :: Worker -> Plan
plan (Worker w) = w.plan

-- | The module's default export: `{ fetch(request, env) }`.
toExport :: Worker -> Foreign
toExport (Worker w) = toExportImpl \env -> do
  handlers <- w.build env
  pure { fetch: fromAff <<< handlers.fetch }

-- | A `wrangler.jsonc` document for this Worker. Hosted classes get a
-- | declarative `exports` entry with SQLite storage; classes hosted elsewhere
-- | get a binding with `script_name`. `assets` is a directory of static
-- | files to serve ahead of the Worker.
wranglerConfig
  :: { name :: String, main :: String, compatibilityDate :: String, assets :: Maybe String }
  -> Worker
  -> Json
wranglerConfig options (Worker w) = J.fromObject $ Object.fromFoldable $
  [ "name" /\ J.fromString options.name
  , "main" /\ J.fromString options.main
  , "compatibility_date" /\ J.fromString options.compatibilityDate
  , "durable_objects" /\ J.fromObject (Object.singleton "bindings" (J.fromArray (bindingJson <$> w.plan.objects)))
  , "exports" /\ J.fromObject (Object.fromFoldable (exportJson <$> hosted))
  ] <> case options.assets of
    Just directory -> [ "assets" /\ J.fromObject (Object.singleton "directory" (J.fromString directory)) ]
    Nothing -> []
  where
  hosted = w.plan.objects # Array.filter (\o -> o.scriptName == Nothing)

  bindingJson o = J.fromObject $ Object.fromFoldable $
    [ "name" /\ J.fromString o.binding, "class_name" /\ J.fromString o.className ]
      <> case o.scriptName of
        Just script -> [ "script_name" /\ J.fromString script ]
        Nothing -> []

  exportJson o = o.className /\ J.fromObject
    (Object.fromFoldable [ "type" /\ J.fromString "durable-object", "storage" /\ J.fromString "sqlite" ])
