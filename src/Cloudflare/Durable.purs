module Cloudflare.Durable
  ( from
  , host
  , module Core
  , module Events
  , module Init
  , module Runtime
  , module Sockets
  ) where

import Prelude

import Cloudflare.Durable.Core (Handlers, Live, Manifest, Namespace, Object, ObjectId, className, emitting, get, getByName, idFromName, idFromString, idToString, implement, implementWith, listen, loopback, manifest, newUniqueId, object, sockets) as Core
import Cloudflare.Durable.Events (Event, Signal(..), event, eventWith) as Events
import Cloudflare.Durable.Sockets (Sockets) as Sockets
import Cloudflare.Durable.Core (Live(..), Object, className)
import Cloudflare.Durable.Init (Init, Plan, state, variable) as Init
import Cloudflare.Durable.Platform (namespaceFromBinding)
import Cloudflare.Durable.Runtime (class MonadRuntime, PlatformError(..), Runtime, Socket, State, liftRuntime) as Runtime
import Cloudflare.Worker (WorkerInit, WorkerRef, objectBinding, scriptName)
import Data.Maybe (Maybe(..))

host :: forall name api events. Live name api events -> WorkerInit (Core.Namespace name api events)
host (Live { object }) = bind' object Nothing

from :: forall name api events. WorkerRef -> Object name api events -> WorkerInit (Core.Namespace name api events)
from worker object = bind' object (Just (scriptName worker))

bind' :: forall name api events. Object name api events -> Maybe String -> WorkerInit (Core.Namespace name api events)
bind' object scriptName = namespaceFromBinding object <$> objectBinding
  { className: className object, binding: className object, scriptName }
