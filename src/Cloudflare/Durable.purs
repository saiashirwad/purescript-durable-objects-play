module Cloudflare.Durable
  ( from
  , host
  , module Core
  , module Init
  , module Runtime
  , module Simulator
  ) where

import Prelude

import Cloudflare.Durable.Core (Handlers, Live, Manifest, Namespace, Object, ObjectId, className, get, getByName, idFromName, idFromString, idToString, implement, implementWith, loopback, manifest, newUniqueId, object) as Core
import Cloudflare.Durable.Core (Live(..), Object, className)
import Cloudflare.Durable.Init (Init, Plan, state, variable) as Init
import Cloudflare.Durable.Platform (namespaceFromBinding)
import Cloudflare.Durable.Runtime (class MonadRuntime, PlatformError(..), Runtime, State, liftRuntime) as Runtime
import Cloudflare.Durable.Simulator (simulate) as Simulator
import Cloudflare.Worker (WorkerInit, WorkerRef, objectBinding, scriptName)
import Data.Maybe (Maybe(..))

host :: forall name api. Live name api -> WorkerInit (Core.Namespace name api)
host (Live { object }) = bind' object Nothing

from :: forall name api. WorkerRef -> Object name api -> WorkerInit (Core.Namespace name api)
from worker object = bind' object (Just (scriptName worker))

bind' :: forall name api. Object name api -> Maybe String -> WorkerInit (Core.Namespace name api)
bind' object scriptName = namespaceFromBinding object <$> objectBinding
  { className: className object, binding: className object, scriptName }
