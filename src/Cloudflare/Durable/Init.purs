module Cloudflare.Durable.Init
  ( Env
  , Init
  , Plan
  , state
  , variable
  , module Static
  ) where

import Prelude

import Cloudflare.Durable.Runtime (Runtime, State, platformError)
import Cloudflare.Static (Static, static)
import Cloudflare.Static (build, plan) as Static
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

type Plan = { variables :: Array String }

type Env = { state :: State, variables :: Map String String }

type Init = Static Plan Env Runtime

state :: Init State
state = static mempty \env -> pure env.state

variable :: String -> Init String
variable name = static { variables: [ name ] } \env ->
  case Map.lookup name env.variables of
    Just value -> pure value
    Nothing -> platformError ("variable " <> show name) "not bound"
