-- | One alarm per object. `schedule` replaces any earlier one. When it is
-- | due the platform runs the object's `alarm` handler (see `implementWith`);
-- | if that fails the platform retries.
module Cloudflare.Durable.Alarm
  ( cancel
  , now
  , schedule
  , scheduleIn
  , scheduled
  ) where

import Prelude

import Cloudflare.Durable.Runtime (class MonadRuntime, State(..), liftRuntime, platform, platformError)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)

now :: forall m. MonadRuntime m => State -> m Instant
now (State s) = liftRuntime $ platform "now" s.now

schedule :: forall m. MonadRuntime m => State -> Instant -> m Unit
schedule (State s) at = liftRuntime $ platform "alarm.set" $ s.setAlarm at

scheduleIn :: forall m. MonadRuntime m => State -> Milliseconds -> m Unit
scheduleIn state delay = do
  current <- now state
  case instant (unInstant current <> delay) of
    Just at -> schedule state at
    Nothing -> liftRuntime $ platformError "alarm.set" "time out of range"

scheduled :: forall m. MonadRuntime m => State -> m (Maybe Instant)
scheduled (State s) = liftRuntime $ platform "alarm.get" s.getAlarm

cancel :: forall m. MonadRuntime m => State -> m Unit
cancel (State s) = liftRuntime $ platform "alarm.delete" s.deleteAlarm
