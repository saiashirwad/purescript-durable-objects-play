module Cloudflare.Durable.Init
  ( Env
  , Image
  , Init
  , InstanceType(..)
  , Plan
  , container
  , instanceTypeName
  , optional
  , state
  , variable
  , module Static
  ) where

import Prelude

import Cloudflare.Durable.Runtime (RawContainer, RawSockets, Runtime, State, platformError)
import Cloudflare.Static (Static, asks, static)
import Cloudflare.Static (build, plan) as Static
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..))
import Data.Show.Generic (genericShow)

-- | What the object asks of the deployment: bound variables, and at most one
-- | container image (`First`: the first declaration wins).
type Plan = { variables :: Array String, container :: First Image }

-- | `Dev` is Cloudflare's old name for `Lite`.
data InstanceType = Lite | Dev | Basic | Standard1 | Standard2 | Standard3 | Standard4

derive instance eqInstanceType :: Eq InstanceType
derive instance genericInstanceType :: Generic InstanceType _

instance showInstanceType :: Show InstanceType where
  show = genericShow

-- | The name wrangler knows the type by.
instanceTypeName :: InstanceType -> String
instanceTypeName = case _ of
  Lite -> "lite"
  Dev -> "lite"
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
state = asks _.state

-- | Declare the image this object runs. The handle comes back typed in
-- | `Cloudflare.Durable.Container`; the image goes into the plan, and from
-- | there into wrangler config.
container :: Image -> Init RawContainer
container image = static ((mempty :: Plan) { container = First (Just image) }) (pure <<< _.container)

-- | Ask for a variable in the plan; `answer` sees what the deployment bound.
bound :: forall a. String -> (Maybe String -> Runtime a) -> Init a
bound name answer = static ((mempty :: Plan) { variables = [ name ] }) (answer <<< Map.lookup name <<< _.variables)

-- | A variable that may be unbound: `Nothing` then, no failure.
optional :: String -> Init (Maybe String)
optional name = bound name pure

variable :: String -> Init String
variable name = bound name $ maybe (platformError ("variable " <> show name) "not bound") pure
