module Cloudflare.Durable.Init
  ( Env
  , Image
  , Init
  , InstanceType(..)
  , Plan
  , container
  , state
  , variable
  , module Static
  ) where

import Prelude

import Cloudflare.Durable.Runtime (RawContainer, RawSockets, Runtime, State, platformError)
import Data.Maybe.First (First(..))
import Cloudflare.Static (Static, static)
import Cloudflare.Static (build, plan) as Static
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

-- | What the object asks of the deployment: bound variables, and at most one
-- | container image (`First`: the first declaration wins).
type Plan = { variables :: Array String, container :: First Image }

data InstanceType = Lite | Dev | Basic | Standard1 | Standard2 | Standard3 | Standard4

derive instance eqInstanceType :: Eq InstanceType

instance showInstanceType :: Show InstanceType where
  show = case _ of
    Lite -> "lite"
    Dev -> "dev"
    Basic -> "basic"
    Standard1 -> "standard-1"
    Standard2 -> "standard-2"
    Standard3 -> "standard-3"
    Standard4 -> "standard-4"

-- | A Dockerfile path or a registry reference, and how many instances of it.
type Image = { image :: String, instances :: Int, instanceType :: InstanceType }

type Env = { state :: State, variables :: Map String String, sockets :: RawSockets, container :: RawContainer }

type Init = Static Plan Env Runtime

state :: Init State
state = static mempty \env -> pure env.state

-- | Declare the image this object runs. The handle comes back typed in
-- | `Cloudflare.Durable.Container`; the image goes into the plan, and from
-- | there into wrangler config.
container :: Image -> Init RawContainer
container image = static { variables: [], container: First (Just image) } \env -> pure env.container

variable :: String -> Init String
variable name = static { variables: [ name ], container: mempty } \env ->
  case Map.lookup name env.variables of
    Just value -> pure value
    Nothing -> platformError ("variable " <> show name) "not bound"
