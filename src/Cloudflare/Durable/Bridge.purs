-- | The Worker entry's view of a `Live` object. From JavaScript:
-- |
-- | ```javascript
-- | import { DurableObject } from "cloudflare:workers";
-- | import { bridge } from "../output/Cloudflare.Durable.Bridge/index.js";
-- | import { counterLive } from "../output/Example.Counter/index.js";
-- |
-- | export class Counter extends bridge(DurableObject, counterLive) {}
-- | ```
-- |
-- | The class activates once under `blockConcurrencyWhile` and exposes one
-- | public method per entry in the manifest. Cloudflare serves public class
-- | methods as RPC methods.
module Cloudflare.Durable.Bridge
  ( bridge
  ) where

import Prelude

import Cloudflare.Durable.Core (Live, activate, manifest)
import Cloudflare.Durable.Platform (stateFromContext, variablesFrom)
import Control.Promise (Promise, fromAff)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Data.Map as Map
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object

type Activate = Foreign -> Foreign -> Effect (Promise (Object (Json -> Effect (Promise Json))))

foreign import bridgeImpl :: Fn3 Foreign (Array String) Activate Foreign

bridge :: forall name api. Fn2 Foreign (Live name api) Foreign
bridge = mkFn2 \base live -> runFn3 bridgeImpl base (manifest live).methods (activateWith live)

activateWith :: forall name api. Live name api -> Activate
activateWith live ctx env = fromAff do
  variables <- liftEffect $ variablesFrom env (manifest live).variables
  handlers <- activate live { state: stateFromContext ctx, variables }
  let promised handle = fromAff <<< handle
  pure $ Object.fromFoldable $ (Map.toUnfoldable (map promised handlers) :: Array _)
