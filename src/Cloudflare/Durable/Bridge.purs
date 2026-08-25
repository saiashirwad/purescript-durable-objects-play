-- | ```javascript
-- | import { DurableObject } from "cloudflare:workers";
-- | export class Counter extends bridge(DurableObject, counterLive) {}
-- | ```
module Cloudflare.Durable.Bridge
  ( bridge
  ) where

import Prelude

import Cloudflare.Durable.Core (Live, activate, manifest)
import Cloudflare.Durable.Platform (containerFromContext, socketsFromContext, stateFromContext, variablesFrom)
import Cloudflare.Worker (Request, Response)
import Cloudflare.Durable.Runtime (Socket)
import Control.Promise (Promise, fromAff)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Data.Map as Map
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object

type Activate =
  Foreign
  -> Foreign
  -> Effect
       ( Promise
           { methods :: Object (Json -> Effect (Promise Json))
           , alarm :: Effect (Promise Unit)
           , connect :: Socket -> Effect (Promise Unit)
           , disconnect :: Socket -> Effect (Promise Unit)
           , fetch :: Request -> Effect (Promise Response)
           }
       )

foreign import bridgeImpl :: Fn3 Foreign (Array String) Activate Foreign

bridge :: forall name api events. Fn2 Foreign (Live name api events) Foreign
bridge = mkFn2 \base live -> runFn3 bridgeImpl base (manifest live).methods (activateWith live)

activateWith :: forall name api events. Live name api events -> Activate
activateWith live ctx env = fromAff do
  variables <- liftEffect $ variablesFrom env (manifest live).variables
  activated <- activate live
    { state: stateFromContext ctx, variables, sockets: socketsFromContext ctx, container: containerFromContext ctx }
  let promised handle = fromAff <<< handle
  pure
    { methods: Object.fromFoldable (Map.toUnfoldable (map promised activated.methods) :: Array _)
    , alarm: fromAff activated.alarm
    , connect: \socket -> fromAff (activated.connect socket)
    , disconnect: \socket -> fromAff (activated.disconnect socket)
    , fetch: \request -> fromAff (activated.fetch request)
    }
