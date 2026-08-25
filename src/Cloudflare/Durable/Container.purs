-- | A container attached to an object. Declare the image during `Init`:
-- |
-- | ```purescript
-- | box <- Durable.container (Container.image "./containers/echo/Dockerfile")
-- | ```
-- |
-- | then in handlers `ensure box port launch`, `request box port`, `stop box`.
-- | `Launch` is a monoid: `env [ "PORT" /\ "8080" ] <> entrypoint [ "node", "server.js" ]`,
-- | and `mempty` is the image's own defaults.
module Cloudflare.Durable.Container
  ( Container
  , Port
  , Stop(..)
  , awaitExit
  , command
  , destroy
  , ensure
  , entrypoint
  , env
  , environment
  , expire
  , fromRaw
  , image
  , internet
  , noInternet
  , renew
  , request
  , running
  , start
  , stop
  , module Exports
  ) where

import Prelude

import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Init (Image, InstanceType(..))
import Cloudflare.Durable.Init (Image, InstanceType(..)) as Exports
import Cloudflare.Durable.Runtime (class MonadRuntime, Exit, Launch(..), RawContainer, State, liftRuntime, platform, platformError)
import Cloudflare.Durable.Runtime (Exit(..), Launch) as Exports
import Cloudflare.Durable.Storage as Storage
import Cloudflare.Worker (Request, Response)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.DateTime.Instant (unInstant)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (over, unwrap)
import Data.Semigroup.Last as Semigroup
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import Data.Tuple (Tuple)
import Effect.Aff (delay)

newtype Container = Container RawContainer

type Port = Int

fromRaw :: RawContainer -> Container
fromRaw = Container

-- | A Dockerfile path (built by wrangler) or a registry reference. Three
-- | `Lite` instances unless you say otherwise.
image :: String -> Image
image path = { image: path, instances: 3, instanceType: Lite }

-- Launch: three ways to say something, three ways to read it back ----------

env :: Array (Tuple String String) -> Launch
env pairs = over Launch _ { env = SemigroupMap $ Semigroup.Last <$> Map.fromFoldable pairs } mempty

entrypoint :: Array String -> Launch
entrypoint argv = over Launch _ { entrypoint = Last (Just argv) } mempty

noInternet :: Launch
noInternet = over Launch _ { internet = Conj false } mempty

environment :: Launch -> Map String String
environment (Launch l) = unwrap <$> unwrap l.env

command :: Launch -> Maybe (Array String)
command (Launch l) = unwrap l.entrypoint

internet :: Launch -> Boolean
internet (Launch l) = unwrap l.internet

-- Lifecycle -----------------------------------------------------------------

running :: forall m. MonadRuntime m => Container -> m Boolean
running (Container c) = liftRuntime $ platform "container.running" c.running

start :: forall m. MonadRuntime m => Container -> Launch -> m Unit
start (Container c) launch = liftRuntime $ platform "container.start" $ c.start launch

-- | Start the container if it is not running, then wait until `port`
-- | answers. Polls four times a second for up to thirty seconds. A running
-- | container is left alone.
ensure :: forall m. MonadRuntime m => Container -> Port -> Launch -> m Unit
ensure box@(Container c) port launch = do
  up <- running box
  unless up do
    start box launch
    ready <- liftRuntime $ platform "container.ensure" $ tailRecM poll 120
    unless ready $ liftRuntime $ platformError "container.ensure" $ "port " <> show port <> " never answered"
  where
  poll :: Int -> _ (Step Int Boolean)
  poll 0 = pure $ Done false
  poll tries = do
    listening <- c.probe port
    alive <- c.running
    if listening then pure $ Done true
    else if not alive then pure $ Done false
    else delay (Milliseconds 250.0) $> Loop (tries - 1)

request :: forall m. MonadRuntime m => Container -> Port -> Request -> m Response
request (Container c) port req = liftRuntime $ platform ("container.request " <> show port) $ c.request port req

-- | The process in the image is PID 1, which ignores `Terminate` and
-- | `Interrupt` unless it installs a handler; `Kill` always works.
data Stop = Terminate | Interrupt | Kill

derive instance eqStop :: Eq Stop

stop :: forall m. MonadRuntime m => Container -> Stop -> m Unit
stop (Container c) how = liftRuntime $ platform "container.stop" $ c.signal case how of
  Terminate -> 15
  Interrupt -> 2
  Kill -> 9

destroy :: forall m. MonadRuntime m => Container -> m Unit
destroy (Container c) = liftRuntime $ platform "container.destroy" c.destroy

-- | Block until the container exits.
awaitExit :: forall m. MonadRuntime m => Container -> m Exit
awaitExit (Container c) = liftRuntime $ platform "container.exit" c.exit

-- Idle timeout: `renew` on use, `expire` from the alarm -----------------------

sleepAtKey :: Storage.Key Number
sleepAtKey = Storage.key "container.sleepAt"

-- | Keep the container for `idle` more; call on every use. Sets the alarm.
renew :: forall m d. MonadRuntime m => Duration d => State -> Container -> d -> m Unit
renew state _ idle = do
  now <- Alarm.now state
  Storage.put state sleepAtKey $ unwrap (unInstant now) + unwrap (fromDuration idle)
  Alarm.scheduleIn state (fromDuration idle)

-- | The alarm half of `renew`: stop the container if its time is up,
-- | otherwise wait for the rest of it. Use as the object's `alarm`.
expire :: forall m. MonadRuntime m => State -> Container -> m Unit
expire state box = do
  now <- unwrap <<< unInstant <$> Alarm.now state
  Storage.get state sleepAtKey >>= case _ of
    Just at | at <= now -> do
      up <- running box
      when up $ stop box Terminate
      void $ Storage.delete state sleepAtKey
    Just at -> Alarm.scheduleIn state $ Milliseconds (at - now)
    Nothing -> pure unit
