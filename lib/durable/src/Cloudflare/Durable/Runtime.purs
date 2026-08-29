module Cloudflare.Durable.Runtime
  ( Exit(..)
  , Launch(..)
  , Listing
  , PlatformError(..)
  , RawContainer
  , RawSockets
  , Runtime
  , Socket
  , State(..)
  , class MonadRuntime
  , decodedOr
  , liftRuntime
  , platform
  , platformError
  , rethrow
  , run
  ) where

import Prelude

import Cloudflare.Worker (Request, Response)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.DateTime.Instant (Instant)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Map (SemigroupMap)
import Data.Maybe (Maybe)
import Data.Maybe.Last (Last)
import Data.Monoid.Conj (Conj)
import Data.Newtype (class Newtype)
import Data.Semigroup.Last as Semigroup
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, attempt, error, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype PlatformError = PlatformError { operation :: String, message :: String }

derive instance eqPlatformError :: Eq PlatformError
derive instance genericPlatformError :: Generic PlatformError _

instance showPlatformError :: Show PlatformError where
  show = genericShow

platformError :: forall m a. MonadThrow PlatformError m => String -> String -> m a
platformError operation message = throwError $ PlatformError { operation, message }

-- | A decode failure, blamed on the platform under `operation`.
decodedOr :: forall a. String -> Either JsonDecodeError a -> Runtime a
decodedOr operation = either (platformError operation <<< printJsonDecodeError) pure

type Listing = { prefix :: String, limit :: Maybe Int, reverse :: Boolean }

-- | Everything one object can ask of the platform. The simulator answers in
-- | memory; the Worker bridge answers with `DurableObjectState`.
newtype State = State
  { get :: String -> Aff (Maybe Json)
  , put :: String -> Json -> Aff Unit
  , delete :: String -> Aff Boolean
  , list :: Listing -> Aff (Array (Tuple String Json))
  , deleteAll :: Aff Unit
  , now :: Aff Instant
  , setAlarm :: Instant -> Aff Unit
  , getAlarm :: Aff (Maybe Instant)
  , deleteAlarm :: Aff Unit
  , sql :: String -> Array Json -> Aff (Array Json)
  }

-- | One open WebSocket. `tag` is what the client connected as.
type Socket = { id :: String, tag :: String }

-- | The untyped socket surface a backend provides; `Cloudflare.Durable.Sockets`
-- | types it with the object's event row.
type RawSockets =
  { broadcast :: Json -> Aff Unit
  , send :: Socket -> Json -> Aff Unit
  , connected :: Aff (Array Socket)
  }

-- | How to start a container. A product of monoids, so the record is one
-- | too: later `env` entries win (`Last`), the last `entrypoint` wins,
-- | internet stays on unless someone turns it off (`Conj`). `mempty` is the
-- | image's own defaults.
newtype Launch = Launch
  { env :: SemigroupMap String (Semigroup.Last String)
  , entrypoint :: Last (Array String)
  , internet :: Conj Boolean
  }

derive instance newtypeLaunch :: Newtype Launch _
derive newtype instance semigroupLaunch :: Semigroup Launch
derive newtype instance monoidLaunch :: Monoid Launch

-- | How a container run ended.
data Exit
  = Exited Int
  | Lost String

derive instance eqExit :: Eq Exit
derive instance genericExit :: Generic Exit _

instance showExit :: Show Exit where
  show = genericShow

-- | The untyped container surface a backend provides. `probe` is true once
-- | something listens on the port.
type RawContainer =
  { running :: Aff Boolean
  , start :: Launch -> Aff Unit
  , probe :: Int -> Aff Boolean
  , request :: Int -> Request -> Aff Response
  , signal :: Int -> Aff Unit
  , destroy :: Aff Unit
  , exit :: Aff Exit
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

-- | Sequence two actions and combine their results. `Runtime Unit` is then a
-- | monoid whose `mempty` does nothing: the shape of an alarm handler.
instance semigroupRuntime :: Semigroup a => Semigroup (Runtime a) where
  append = lift2 append

instance monoidRuntime :: Monoid a => Monoid (Runtime a) where
  mempty = pure mempty

run :: forall a. Runtime a -> Aff (Either PlatformError a)
run (Runtime action) = runExceptT action

-- | Into plain `Aff`, a `PlatformError` becoming an exception the platform
-- | sees (and retries an alarm for).
rethrow :: Runtime ~> Aff
rethrow action = run action >>= either (throwError <<< error <<< show) pure

platform :: forall a. String -> Aff a -> Runtime a
platform operation action = Runtime $ ExceptT $
  lmap (\exception -> PlatformError { operation, message: message exception }) <$> attempt action

class Monad m <= MonadRuntime m where
  liftRuntime :: forall a. Runtime a -> m a

instance monadRuntimeRuntime :: MonadRuntime Runtime where
  liftRuntime = identity
