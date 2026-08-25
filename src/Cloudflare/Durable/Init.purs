-- | The init phase. `Init` is an applicative, not a monad, on purpose: every
-- | request it makes is visible without running anything, so `plan` can list
-- | an object's needs at build time and `build` can satisfy them at runtime.
-- | Use `ado` to combine requests.
module Cloudflare.Durable.Init
  ( Env
  , Init
  , Plan
  , build
  , plan
  , state
  , variable
  ) where

import Prelude

import Cloudflare.Durable.Runtime (Runtime, State, platformError)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

-- | What an object needs from its deployment.
type Plan = { variables :: Array String }

-- | What the runtime gives an object when it activates.
type Env = { state :: State, variables :: Map String String }

newtype Init a = Init { plan :: Plan, build :: Env -> Runtime a }

instance functorInit :: Functor Init where
  map f (Init i) = Init { plan: i.plan, build: map f <<< i.build }

instance applyInit :: Apply Init where
  apply (Init f) (Init x) = Init
    { plan: { variables: f.plan.variables <> x.plan.variables }
    , build: \env -> f.build env <*> x.build env
    }

instance applicativeInit :: Applicative Init where
  pure a = Init { plan: { variables: [] }, build: \_ -> pure a }

-- | The object's state handle. It is safe to hold in `Init`; its operations
-- | run in `Runtime` or `Rpc`.
state :: Init State
state = Init { plan: { variables: [] }, build: \env -> pure env.state }

-- | A Worker environment variable by name.
variable :: String -> Init String
variable name = Init
  { plan: { variables: [ name ] }
  , build: \env -> case Map.lookup name env.variables of
      Just value -> pure value
      Nothing -> platformError ("variable " <> show name) "not bound"
  }

plan :: forall a. Init a -> Plan
plan (Init i) = i.plan

build :: forall a. Init a -> Env -> Runtime a
build (Init i) = i.build
