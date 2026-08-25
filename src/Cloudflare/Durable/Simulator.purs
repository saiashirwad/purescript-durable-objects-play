-- | An in-memory backend. Each id gets its own instance and its own storage,
-- | activated on first use. Calls cross the codecs and the response envelope
-- | exactly as they do over the wire, so tests exercise the real protocol.
module Cloudflare.Durable.Simulator
  ( simulate
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Live, Namespace, activate, namespace)
import Cloudflare.Durable.Core as Core
import Cloudflare.Durable.Runtime (State(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

simulate :: forall name api. Live name api -> Aff (Namespace name api)
simulate live@(Core.Live { object }) = do
  instances <- liftEffect $ Ref.new Map.empty
  counter <- liftEffect $ Ref.new 0
  let
    className = Core.className object

    freshState = liftEffect do
      storage <- Ref.new Map.empty
      pure $ State
        { get: \key -> liftEffect $ Map.lookup key <$> Ref.read storage
        , put: \key value -> liftEffect $ Ref.modify_ (Map.insert key value) storage
        , delete: \key -> liftEffect do
            entries <- Ref.read storage
            Ref.write (Map.delete key entries) storage
            pure $ Map.member key entries
        }

    instanceFor id = do
      existing <- liftEffect $ Map.lookup id <$> Ref.read instances
      case existing of
        Just handlers -> pure handlers
        Nothing -> do
          state <- freshState
          handlers <- activate live { state, variables: Map.empty }
          liftEffect $ Ref.modify_ (Map.insert id handlers) instances
          pure handlers

    call id methodName request = do
      handlers <- instanceFor id
      case Map.lookup methodName handlers of
        Just handle -> handle request
        Nothing -> throwError $ error $ className <> " has no method " <> show methodName

    unique = liftEffect do
      n <- Ref.modify (_ + 1) counter
      pure $ Unique $ className <> "-" <> show n

  pure $ namespace object { call, unique }
