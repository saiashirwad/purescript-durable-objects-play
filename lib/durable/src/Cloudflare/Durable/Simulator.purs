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

import Cloudflare.Durable.Core (Activated, Id(..), Listener, Live(..), Namespace, activate, className, dispatch, namespace)
import Cloudflare.Durable.Events (Signal(..))
import Cloudflare.Durable.Runtime (Exit(..), Launch, RawContainer, RawSockets, Socket, State(..))
import Cloudflare.Worker (Request, Response)
import Control.Monad.Rec.Class (untilJust)
import Data.Argonaut.Core (Json)
import Data.Array (reverse, take)
import Data.Array as Array
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Time.Duration (Milliseconds(..))
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

-- Time -----------------------------------------------------------------------

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

-- Entry points -----------------------------------------------------------------

simulate :: forall name api events. Live name api events -> Aff (Namespace name api events)
simulate live = clock >>= \c -> simulateOn c live

-- | A stand-in for the container image: what answers on each port once the
-- | container is "started". Start and stop only flip a flag.
-- | `variables` are what `Durable.variable` / `optional` see.
type Stub = { serve :: Int -> Request -> Aff Response, launched :: Launch -> Aff Unit, variables :: Map String String }

noContainer :: Stub
noContainer =
  { serve: \port _ -> throwError $ error $ "nothing listens on port " <> show port <> "; give the simulator a Stub"
  , launched: \_ -> pure unit
  , variables: Map.empty
  }

simulateOn :: forall name api events. Clock -> Live name api events -> Aff (Namespace name api events)
simulateOn = simulateWith noContainer

-- One instance = one id's worth of platform ---------------------------------------

type Alarm = Ref (Maybe Instant)

type Book = Ref (Map String { socket :: Socket, deliver :: Listener })

-- | Storage, SQLite and the alarm slot, all in memory; the clock is shared.
memoryState :: Clock -> Effect { state :: State, alarm :: Alarm }
memoryState (Clock c) = do
  storage <- Ref.new Map.empty
  db <- openMemory
  alarm <- Ref.new Nothing
  let
    state = State
      { get: \key -> liftEffect $ Map.lookup key <$> Ref.read storage
      , put: \key value -> liftEffect $ Ref.modify_ (Map.insert key value) storage
      , delete: \key -> liftEffect $ Ref.modify' (\entries -> { state: Map.delete key entries, value: Map.member key entries }) storage
      , list: \options -> liftEffect do
          matching <- Map.toUnfoldable <<< Map.filterKeys (hasPrefix options.prefix) <$> Ref.read storage
          let ordered = if options.reverse then reverse matching else matching
          pure $ maybe identity take options.limit ordered
      , deleteAll: liftEffect $ Ref.write Map.empty storage *> dropAll db
      , now: liftEffect $ Ref.read c.time
      , setAlarm: \at -> liftEffect $ Ref.write (Just at) alarm
      , getAlarm: liftEffect $ Ref.read alarm
      , deleteAlarm: liftEffect $ Ref.write Nothing alarm
      , sql: \text bindings -> liftEffect $ exec db text bindings
      }
  pure { state, alarm }
  where
  hasPrefix p = isJust <<< stripPrefix (Pattern p)

-- | Open sockets, each a listener to deliver to.
socketBook :: Effect { sockets :: RawSockets, book :: Book }
socketBook = do
  book <- Ref.new Map.empty
  let
    deliverTo json { deliver } = deliver (Delivered json)
    sockets =
      { broadcast: \json -> liftEffect $ Ref.read book >>= traverse_ (deliverTo json)
      , send: \socket json -> liftEffect $ Ref.read book >>= Map.lookup socket.id >>> traverse_ (deliverTo json)
      , connected: liftEffect $ Array.fromFoldable <<< map _.socket <$> Ref.read book
      }
  pure { sockets, book }

-- | A container that is a flag: `start` raises it, any signal lowers it.
fakeContainer :: Stub -> Effect RawContainer
fakeContainer stub = do
  up <- Ref.new false
  exited <- Ref.new Nothing
  let
    halt code = liftEffect $ Ref.write false up *> Ref.write (Just (Exited code)) exited
  pure
    { running: liftEffect $ Ref.read up
    , start: \launch -> do
        liftEffect $ Ref.write true up *> Ref.write Nothing exited
        stub.launched launch
    , probe: \_ -> liftEffect $ Ref.read up
    , request: \port req -> do
        alive <- liftEffect $ Ref.read up
        if alive then stub.serve port req else throwError $ error "container port not listening"
    , signal: \code -> halt (128 + code)
    , destroy: halt 137
    , exit: untilJust do
        done <- liftEffect $ Ref.read exited
        when (isNothing done) $ delay (Milliseconds 20.0)
        pure done
    }

type Instance = { activated :: Activated, alarm :: Alarm, book :: Book }

-- The namespace ---------------------------------------------------------------------

simulateWith :: forall name api events. Stub -> Clock -> Live name api events -> Aff (Namespace name api events)
simulateWith stub timeline@(Clock c) live@(Live { object }) = do
  instances <- liftEffect $ Ref.new Map.empty
  counter <- liftEffect $ Ref.new 0
  let
    name = className object

    fresh :: Aff Instance
    fresh = do
      { state, alarm } <- liftEffect $ memoryState timeline
      { sockets, book } <- liftEffect socketBook
      container <- liftEffect $ fakeContainer stub
      activated <- activate live { state, variables: stub.variables, sockets, container }
      pure { activated, alarm, book }

    instanceFor id = liftEffect (Map.lookup id <$> Ref.read instances) >>= case _ of
      Just found -> pure found
      Nothing -> do
        created <- fresh
        liftEffect $ Ref.modify_ (Map.insert id created) instances
        pure created

    call id methodName request = do
      { activated } <- instanceFor id
      dispatch name activated.methods methodName request

    unique = liftEffect do
      n <- Ref.modify (_ + 1) counter
      pure $ Unique $ name <> "-" <> show n

    listen id tag deliver = do
      n <- Ref.modify (_ + 1) counter
      let socket = { id: name <> "-socket-" <> show n, tag }
      launchAff_ do
        { activated, book } <- instanceFor id
        liftEffect $ Ref.modify_ (Map.insert socket.id { socket, deliver }) book
        liftEffect $ deliver Opened
        activated.connect socket
      pure $ launchAff_ do
        { activated, book } <- instanceFor id
        liftEffect $ Ref.modify_ (Map.delete socket.id) book
        liftEffect $ deliver Closed
        activated.disconnect socket

    fetch id request = do
      { activated } <- instanceFor id
      activated.fetch request

    wake at = do
      all <- liftEffect $ Ref.read instances
      for_ all \{ activated, alarm } -> do
        due <- liftEffect $ Ref.read alarm
        for_ due \at' -> when (at' <= at) do
          liftEffect $ Ref.write Nothing alarm
          activated.alarm

  liftEffect $ Ref.modify_ (_ <> [ wake ]) c.wakers
  pure $ namespace object { call, unique, listen, fetch }
