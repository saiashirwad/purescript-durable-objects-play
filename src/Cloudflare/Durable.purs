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

import Cloudflare.Durable.Core (Handlers, Live, Manifest, Namespace, Object, ObjectId, className, container, emitting, get, getByName, handlers, http, idFromName, idFromString, idToString, implement, implementWith, listen, loopback, manifest, newUniqueId, object, sockets) as Core
import Cloudflare.Durable.Events (Event, Signal(..), event, eventWith) as Events
import Cloudflare.Durable.Sockets (Sockets) as Sockets
import Cloudflare.Durable.Core (Live(..), Object, className, manifest)
import Cloudflare.Durable.Init (Init, Plan, optional, state, variable) as Init
import Cloudflare.Durable.Platform (namespaceFromBinding)
import Cloudflare.Durable.Runtime (class MonadRuntime, PlatformError(..), Runtime, Socket, State, liftRuntime) as Runtime
import Cloudflare.Worker (ContainerSpec, WorkerInit, WorkerRef, objectBinding, scriptName)
import Data.Maybe (Maybe(..))

-- | Host an object in this Worker: the binding, the export, and any
-- | container image all follow from the `Live`.
host :: forall name api events. Live name api events -> WorkerInit (Core.Namespace name api events)
host live@(Live { object }) = bound object Nothing $ (manifest live).container <#> \i ->
  { image: i.image, instances: i.instances, instanceType: show i.instanceType }

-- | Reach an object hosted by another Worker.
from :: forall name api events. WorkerRef -> Object name api events -> WorkerInit (Core.Namespace name api events)
from worker object = bound object (Just (scriptName worker)) Nothing

bound :: forall name api events. Object name api events -> Maybe String -> Maybe ContainerSpec -> WorkerInit (Core.Namespace name api events)
bound object scriptName container = namespaceFromBinding object <$> objectBinding
  { className: className object, binding: className object, scriptName, container }
