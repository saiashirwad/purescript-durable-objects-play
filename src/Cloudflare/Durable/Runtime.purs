-- | The runtime phase. `Runtime` is where platform work happens: storage,
-- | alarms, and anything else that can fail for reasons outside the program.
-- | `Rpc e` (in `Cloudflare.Durable.Rpc`) can run any `Runtime` action through
-- | `MonadRuntime`, so platform helpers work during activation and inside calls.
module Cloudflare.Durable.Runtime
  ( PlatformError(..)
  , Runtime
  , State(..)
  , class MonadRuntime
  , liftRuntime
  , platform
  , platformError
  , run
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, attempt, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

-- | A failure of the platform, not of the application.
data PlatformError = PlatformError { operation :: String, message :: String }

instance showPlatformError :: Show PlatformError where
  show (PlatformError { operation, message }) =
    "PlatformError { operation: " <> show operation <> ", message: " <> show message <> " }"

derive instance eqPlatformError :: Eq PlatformError

platformError :: forall m a. MonadThrow PlatformError m => String -> String -> m a
platformError operation message = throwError $ PlatformError { operation, message }

-- | The handle to one object's durable state. Obtain it with
-- | `Cloudflare.Durable.state` during `Init`; use it in `Runtime` or `Rpc`.
-- | The record is the storage interface a backend provides: the simulator
-- | keeps a map in memory, the Worker bridge wraps `DurableObjectState`.
newtype State = State
  { get :: String -> Aff (Maybe Json)
  , put :: String -> Json -> Aff Unit
  , delete :: String -> Aff Boolean
  }

newtype Runtime a = Runtime (ExceptT PlatformError Aff a)

derive newtype instance functorRuntime :: Functor Runtime
derive newtype instance applyRuntime :: Apply Runtime
derive newtype instance applicativeRuntime :: Applicative Runtime
derive newtype instance bindRuntime :: Bind Runtime
derive newtype instance monadRuntime :: Monad Runtime
derive newtype instance monadRecRuntime :: MonadRec Runtime
derive newtype instance monadEffectRuntime :: MonadEffect Runtime
derive newtype instance monadAffRuntime :: MonadAff Runtime
derive newtype instance monadThrowRuntime :: MonadThrow PlatformError Runtime
derive newtype instance monadErrorRuntime :: MonadError PlatformError Runtime

run :: forall a. Runtime a -> Aff (Either PlatformError a)
run (Runtime action) = runExceptT action

-- | Run a platform call. An exception becomes a `PlatformError` naming the
-- | operation.
platform :: forall a. String -> Aff a -> Runtime a
platform operation action = Runtime $ ExceptT $
  lmap (\exception -> PlatformError { operation, message: message exception }) <$> attempt action

-- | Monads that can perform platform work.
class Monad m <= MonadRuntime m where
  liftRuntime :: forall a. Runtime a -> m a

instance monadRuntimeRuntime :: MonadRuntime Runtime where
  liftRuntime = identity
