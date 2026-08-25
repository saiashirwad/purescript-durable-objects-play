-- | An in-memory Cloudflare. Each id gets its own instance, storage, SQLite
-- | database, alarm and sockets, activated on first use. Calls cross the
-- | codecs and the response envelope exactly as they do over the wire.
-- |
-- | Time is a `Clock` you advance by hand; due alarms fire during `advance`.
-- | Objects sharing a clock share a timeline.
module Cloudflare.Durable.Simulator
  ( Clock
  , Stub
  , advance
  , clock
  , noContainer
  , simulate
  , simulateOn
  , simulateWith
  ) where

import Prelude

import Cloudflare.Durable.Core (Activated, Id(..), Listener, Live, Namespace, activate, namespace)
import Cloudflare.Durable.Core as Core
import Cloudflare.Durable.Events (Signal(..))
import Cloudflare.Durable.Runtime (Exit(..), Launch, Socket, State(..))
import Cloudflare.Worker (Request, Response)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Time.Duration (Milliseconds(..))
import Data.Argonaut.Core (Json)
import Data.Array (filter, reverse, take)
import Data.Array as Array
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, error, launchAff_, throwError)
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

simulate :: forall name api events. Live name api events -> Aff (Namespace name api events)
simulate live = clock >>= \c -> simulateOn c live

-- | A stand-in for the container image: what answers on each port once the
-- | container is "started". Start and stop only flip a flag.
type Stub = { serve :: Int -> Request -> Aff Response, launched :: Launch -> Aff Unit }

noContainer :: Stub
noContainer =
  { serve: \port _ -> throwError $ error $ "nothing listens on port " <> show port <> "; give the simulator a Stub"
  , launched: \_ -> pure unit
  }

simulateOn :: forall name api events. Clock -> Live name api events -> Aff (Namespace name api events)
simulateOn = simulateWith noContainer

type Instance =
  { activated :: Activated
  , alarm :: Ref (Maybe Instant)
  , sockets :: Ref (Map String { socket :: Socket, deliver :: Listener })
  }

simulateWith :: forall name api events. Stub -> Clock -> Live name api events -> Aff (Namespace name api events)
simulateWith stub (Clock c) live@(Core.Live { object }) = do
  instances <- liftEffect $ Ref.new Map.empty
  counter <- liftEffect $ Ref.new 0
  let
    className = Core.className object

    fresh = liftEffect do
      storage <- Ref.new Map.empty
      db <- openMemory
      alarm <- Ref.new Nothing
      sockets <- Ref.new Map.empty
      let
        each f = liftEffect $ Ref.read sockets >>= traverse_ f
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
        raw =
          { broadcast: \json -> each \{ deliver } -> deliver (Delivered json)
          , send: \socket json -> liftEffect $ Ref.read sockets >>= Map.lookup socket.id >>> traverse_ \{ deliver } -> deliver (Delivered json)
          , connected: liftEffect $ map _.socket <<< Map.values >>> Array.fromFoldable <$> Ref.read sockets
          }
      up <- Ref.new false
      exited <- Ref.new Nothing
      let
        halt code = liftEffect do
          Ref.write false up
          Ref.write (Just (Exited code)) exited
        box =
          { running: liftEffect $ Ref.read up
          , start: \launch -> do
              liftEffect $ Ref.write true up
              liftEffect $ Ref.write Nothing exited
              stub.launched launch
          , probe: \_ -> liftEffect $ Ref.read up
          , request: \port req -> do
              alive <- liftEffect $ Ref.read up
              if alive then stub.serve port req else throwError $ error "container port not listening"
          , signal: \code -> halt (128 + code)
          , destroy: halt 137
          , exit: tailRecM (\_ -> liftEffect (Ref.read exited) >>= case _ of
              Just outcome -> pure $ Done outcome
              Nothing -> delay (Milliseconds 20.0) $> Loop unit) unit
          }
      pure { state, raw, box, alarm, sockets }

    instanceFor id = do
      existing <- liftEffect $ Map.lookup id <$> Ref.read instances
      case existing of
        Just found -> pure found
        Nothing -> do
          { state, raw, box, alarm, sockets } <- fresh
          activated <- activate live { state, variables: Map.empty, sockets: raw, container: box }
          let created = { activated, alarm, sockets }
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

    listen id tag deliver = do
      n <- Ref.modify (_ + 1) counter
      let socket = { id: className <> "-socket-" <> show n, tag }
      launchAff_ do
        { activated, sockets } <- instanceFor id
        liftEffect $ Ref.modify_ (Map.insert socket.id { socket, deliver }) sockets
        liftEffect $ deliver Opened
        activated.connect socket
      pure $ launchAff_ do
        { activated, sockets } <- instanceFor id
        liftEffect $ Ref.modify_ (Map.delete socket.id) sockets
        liftEffect $ deliver Closed
        activated.disconnect socket

    fetch id request = do
      { activated } <- instanceFor id
      activated.fetch request

    wake at = do
      all <- liftEffect $ Map.values <$> Ref.read instances
      for_ all \{ activated, alarm } -> do
        due <- liftEffect $ Ref.read alarm
        for_ due \at' -> when (at' <= at) do
          liftEffect $ Ref.write Nothing alarm
          activated.alarm

  liftEffect $ Ref.modify_ (_ <> [ wake ]) c.wakers
  pure $ namespace object { call, unique, listen, fetch }
  where
  hasPrefix p key = p == "" || stripPrefix (Pattern p) key /= Nothing
