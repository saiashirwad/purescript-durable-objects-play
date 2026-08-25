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
  , liftRuntime
  , platform
  , platformError
  , run
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Cloudflare.Worker (Request, Response)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe.Last (Last)
import Data.Monoid.Conj (Conj)
import Effect.Aff (Aff, attempt, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

data PlatformError = PlatformError { operation :: String, message :: String }

instance showPlatformError :: Show PlatformError where
  show (PlatformError { operation, message }) =
    "PlatformError { operation: " <> show operation <> ", message: " <> show message <> " }"

derive instance eqPlatformError :: Eq PlatformError

platformError :: forall m a. MonadThrow PlatformError m => String -> String -> m a
platformError operation message = throwError $ PlatformError { operation, message }

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

-- | How to start a container. A monoid: `env` unions, the last `entrypoint`
-- | wins, internet stays on unless someone turns it off. `mempty` is the
-- | image's own defaults.
newtype Launch = Launch
  { env :: Map String String
  , entrypoint :: Last (Array String)
  , internet :: Conj Boolean
  }

instance semigroupLaunch :: Semigroup Launch where
  append (Launch a) (Launch b) = Launch
    { env: Map.union b.env a.env
    , entrypoint: a.entrypoint <> b.entrypoint
    , internet: a.internet <> b.internet
    }

instance monoidLaunch :: Monoid Launch where
  mempty = Launch { env: Map.empty, entrypoint: mempty, internet: mempty }

-- | How a container run ended.
data Exit
  = Exited Int
  | Lost String

derive instance eqExit :: Eq Exit

instance showExit :: Show Exit where
  show = case _ of
    Exited code -> "(Exited " <> show code <> ")"
    Lost why -> "(Lost " <> show why <> ")"

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

platform :: forall a. String -> Aff a -> Runtime a
platform operation action = Runtime $ ExceptT $
  lmap (\exception -> PlatformError { operation, message: message exception }) <$> attempt action

class Monad m <= MonadRuntime m where
  liftRuntime :: forall a. Runtime a -> m a

instance monadRuntimeRuntime :: MonadRuntime Runtime where
  liftRuntime = identity
