-- | An in-memory Cloudflare. Each id gets its own instance, storage, SQLite
-- | database and alarm, activated on first use. Calls cross the codecs and
-- | the response envelope exactly as they do over the wire.
-- |
-- | Time is a `Clock` you advance by hand; due alarms fire during `advance`.
-- | Objects sharing a clock share a timeline.
module Cloudflare.Durable.Simulator
  ( Clock
  , advance
  , clock
  , simulate
  , simulateOn
  ) where

import Prelude

import Cloudflare.Durable.Core (Activated, Id(..), Live, Namespace, activate, namespace)
import Cloudflare.Durable.Core as Core
import Cloudflare.Durable.Runtime (State(..))
import Data.Argonaut.Core (Json)
import Data.Array (filter, reverse, take)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref

foreign import data Database :: Type
foreign import openMemory :: Effect Database
foreign import exec :: Database -> String -> Array Json -> Effect (Array Json)
foreign import dropAll :: Database -> Effect Unit

newtype Clock = Clock
  { time :: Ref Instant
  , wakers :: Ref (Array (Instant -> Aff Unit))
  }

clock :: Aff Clock
clock = liftEffect do
  start <- now
  time <- Ref.new start
  wakers <- Ref.new []
  pure $ Clock { time, wakers }

-- | Move the clock forward and fire every alarm now due, each at most once.
advance :: Clock -> Milliseconds -> Aff Unit
advance (Clock c) delta = do
  later <- liftEffect do
    current <- Ref.read c.time
    let later = fromMaybe current $ instant (unInstant current <> delta)
    Ref.write later c.time
    pure later
  wakers <- liftEffect $ Ref.read c.wakers
  traverse_ (_ $ later) wakers

simulate :: forall name api. Live name api -> Aff (Namespace name api)
simulate live = clock >>= \c -> simulateOn c live

type Instance = { activated :: Activated, alarm :: Ref (Maybe Instant) }

simulateOn :: forall name api. Clock -> Live name api -> Aff (Namespace name api)
simulateOn (Clock c) live@(Core.Live { object }) = do
  instances <- liftEffect $ Ref.new Map.empty
  counter <- liftEffect $ Ref.new 0
  let
    className = Core.className object

    freshState = liftEffect do
      storage <- Ref.new Map.empty
      db <- openMemory
      alarm <- Ref.new Nothing
      let
        state = State
          { get: \key -> liftEffect $ Map.lookup key <$> Ref.read storage
          , put: \key value -> liftEffect $ Ref.modify_ (Map.insert key value) storage
          , delete: \key -> liftEffect do
              entries <- Ref.read storage
              Ref.write (Map.delete key entries) storage
              pure $ Map.member key entries
          , list: \options -> liftEffect do
              entries <- Map.toUnfoldable <$> Ref.read storage
              let matching = filter (\(Tuple key _) -> hasPrefix options.prefix key) entries
              let ordered = if options.reverse then reverse matching else matching
              pure $ maybe ordered (_ `take` ordered) options.limit
          , deleteAll: liftEffect do
              Ref.write Map.empty storage
              dropAll db
          , now: liftEffect $ Ref.read c.time
          , setAlarm: \at -> liftEffect $ Ref.write (Just at) alarm
          , getAlarm: liftEffect $ Ref.read alarm
          , deleteAlarm: liftEffect $ Ref.write Nothing alarm
          , sql: \text bindings -> liftEffect $ exec db text bindings
          }
      pure { state, alarm }

    instanceFor id = do
      existing <- liftEffect $ Map.lookup id <$> Ref.read instances
      case existing of
        Just found -> pure found
        Nothing -> do
          { state, alarm } <- freshState
          activated <- activate live { state, variables: Map.empty }
          let created = { activated, alarm }
          liftEffect $ Ref.modify_ (Map.insert id created) instances
          pure created

    call id methodName request = do
      { activated } <- instanceFor id
      case Map.lookup methodName activated.methods of
        Just handle -> handle request
        Nothing -> throwError $ error $ className <> " has no method " <> show methodName

    unique = liftEffect do
      n <- Ref.modify (_ + 1) counter
      pure $ Unique $ className <> "-" <> show n

    wake at = do
      all <- liftEffect $ Map.values <$> Ref.read instances
      for_ all \{ activated, alarm } -> do
        due <- liftEffect $ Ref.read alarm
        for_ due \at' -> when (at' <= at) do
          liftEffect $ Ref.write Nothing alarm
          activated.alarm

  liftEffect $ Ref.modify_ (_ <> [ wake ]) c.wakers
  pure $ namespace object { call, unique }
  where
  hasPrefix p key = p == "" || stripPrefix (Pattern p) key /= Nothing
