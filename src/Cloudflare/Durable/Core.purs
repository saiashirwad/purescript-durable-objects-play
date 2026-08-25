module Cloudflare.Durable.Core
  ( Activated
  , Handlers
  , Id(..)
  , Listener
  , Live(..)
  , Manifest
  , Namespace(..)
  , Object(..)
  , ObjectId(..)
  , Transport
  , activate
  , className
  , container
  , emitting
  , get
  , getByName
  , handlers
  , http
  , idFromName
  , idFromString
  , idToString
  , implement
  , implementWith
  , listen
  , loopback
  , manifest
  , namespace
  , newUniqueId
  , object
  , printId
  , sockets
  ) where

import Prelude

import Cloudflare.Durable.Container (Container)
import Cloudflare.Durable.Container as Container
import Cloudflare.Durable.Events (class DecodeEvents, class EncodeEvents, Signal, decodeEvents, decoded, encodeEvents, unwire)
import Cloudflare.Durable.Init (Env, Image, Init)
import Cloudflare.Durable.Init as Init
import Cloudflare.Durable.Protocol (class Connect, class MethodNames, class Serve, RawCall, RawHandler, connect, methodNames, serve)
import Cloudflare.Durable.Runtime (Runtime, Socket)
import Cloudflare.Durable.Runtime as Runtime
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Static (asks)
import Cloudflare.Worker (Request, Response)
import Cloudflare.Worker as Worker
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

-- Objects -------------------------------------------------------------------

-- | The contract of one Durable Object class: its name, and both ends of
-- | every method (`serve` for the object, `connect` for its callers), plus
-- | both ends of every event.
newtype Object :: Symbol -> Row Type -> Row Type -> Type
newtype Object name api events = Object
  { name :: String
  , methods :: Array String
  , serve :: Record api -> Map String RawHandler
  , connect :: RawCall -> Record api
  , encodeEvent :: Variant events -> Json
  , decodeEvent :: Json -> Either JsonDecodeError (Variant events)
  }

-- | The class name comes from the signature, `counter :: Object "Counter" CounterApi ()`.
-- | Each `method` field takes its types from the API row.
object
  :: forall name spec list api
   . IsSymbol name
  => RowToList spec list
  => MethodNames list
  => Connect list spec api
  => Serve list spec api
  => Record spec
  -> Object name api ()
object spec = Object
  { name: reflectSymbol (Proxy :: Proxy name)
  , methods: methodNames list
  , serve: serve list spec
  , connect: connect list spec
  , encodeEvent: case_
  , decodeEvent: \_ -> Left $ CA.Named "event" $ CA.TypeMismatch "this object emits no events"
  }
  where
  list = Proxy :: Proxy list

-- | Give an object an event row: `room = object { ... } `emitting` { message: event }`.
-- | The row is read off the signature, as with methods.
emitting
  :: forall name api spec list events
   . RowToList spec list
  => EncodeEvents list spec events
  => DecodeEvents list spec events
  => Object name api ()
  -> Record spec
  -> Object name api events
emitting (Object o) spec = Object o
  { encodeEvent = encodeEvents list spec
  , decodeEvent = \json -> do
      Tuple tag value <- unwire json
      fromMaybe (Left $ CA.Named "event" $ CA.UnexpectedValue $ J.fromString tag) $ decodeEvents list spec tag value
  }
  where
  list = Proxy :: Proxy list

className :: forall name api events. Object name api events -> String
className (Object o) = o.name

-- | `connect` after `serve`: an implementation seen through the wire. The
-- | simulator is this, one per id; the tests check it behaves as the
-- | implementation.
loopback :: forall name api events. Object name api events -> Record api -> Record api
loopback (Object o) impl = o.connect \name request ->
  case Map.lookup name (o.serve impl) of
    Just handle -> handle request
    Nothing -> throwError $ error $ o.name <> " has no method " <> show name

-- Implementations ------------------------------------------------------------

-- | What activation yields. `alarm` runs when a scheduled alarm is due;
-- | `connect` and `disconnect` run as sockets come and go; each is a monoid
-- | whose `mempty` does nothing. `fetch` answers plain HTTP sent to the
-- | object at `<prefix>/<class>/<id>/http/...`; `noFetch` answers 404.
type Handlers api =
  { methods :: Record api
  , alarm :: Runtime Unit
  , connect :: Socket -> Runtime Unit
  , disconnect :: Socket -> Runtime Unit
  , fetch :: Request -> Runtime Response
  }

noFetch :: Request -> Runtime Response
noFetch _ = pure $ Worker.text 404 "this object serves no HTTP"

-- | Methods with every hook at its default. Override with record update:
-- | `(handlers methods) { alarm = ..., fetch = ... }`.
handlers :: forall api. Record api -> Handlers api
handlers methods = { methods, alarm: mempty, connect: mempty, disconnect: mempty, fetch: noFetch }

-- | The object's sockets, typed by its event row. Use during `Init` like `state`.
sockets :: forall name api events. Object name api events -> Init (Sockets (Variant events))
sockets (Object o) = asks $ Sockets.fromRaw o.encodeEvent <<< _.sockets

-- | The container declared with an `Image`, as a typed handle.
container :: Image -> Init Container
container = map Container.fromRaw <<< Init.container

-- | An object and its implementation: an `Init` that plans what the object
-- | needs, then a `Runtime` that builds its handlers once per activation.
newtype Live :: Symbol -> Row Type -> Row Type -> Type
newtype Live name api events = Live
  { object :: Object name api events
  , activate :: Init (Runtime (Handlers api))
  }

implement :: forall name api events. Object name api events -> Init (Runtime (Record api)) -> Live name api events
implement o = implementWith o <<< map (map handlers)

implementWith :: forall name api events. Object name api events -> Init (Runtime (Handlers api)) -> Live name api events
implementWith o activation = Live { object: o, activate: activation }

type Manifest = { className :: String, methods :: Array String, variables :: Array String, container :: Maybe Image }

manifest :: forall name api events. Live name api events -> Manifest
manifest (Live { object: Object o, activate: activation }) =
  { className: o.name, methods: o.methods, variables: plan.variables, container: unwrap plan.container }
  where
  plan = Init.plan activation

-- | The handlers as the platform calls them: untyped methods, and hooks in
-- | plain `Aff` whose failures are exceptions.
type Activated =
  { methods :: Map String RawHandler
  , alarm :: Aff Unit
  , connect :: Socket -> Aff Unit
  , disconnect :: Socket -> Aff Unit
  , fetch :: Request -> Aff Response
  }

activate :: forall name api events. Live name api events -> Env -> Aff Activated
activate (Live { object: Object o, activate: activation }) env =
  Runtime.run (join $ Init.build activation env) >>= case _ of
    Right active -> pure
      { methods: o.serve active.methods
      , alarm: Runtime.rethrow active.alarm
      , connect: Runtime.rethrow <<< active.connect
      , disconnect: Runtime.rethrow <<< active.disconnect
      , fetch: Runtime.rethrow <<< active.fetch
      }
    Left failure -> throwError $ error $ o.name <> " failed to activate: " <> show failure

-- Ids ----------------------------------------------------------------------

data Id
  = Named String
  | Unique String

derive instance eqId :: Eq Id
derive instance ordId :: Ord Id

instance showId :: Show Id where
  show (Named name) = "(Named " <> show name <> ")"
  show (Unique id) = "(Unique " <> show id <> ")"

-- | Unique ids print as hex; named ids print as their name.
printId :: Id -> String
printId = case _ of
  Unique hex -> hex
  Named name -> name

newtype ObjectId :: Symbol -> Type
newtype ObjectId name = ObjectId Id

derive newtype instance eqObjectId :: Eq (ObjectId name)
derive newtype instance ordObjectId :: Ord (ObjectId name)
derive newtype instance showObjectId :: Show (ObjectId name)

-- Namespaces ----------------------------------------------------------------

type Listener = Signal Json -> Effect Unit

-- | How a namespace reaches its objects. `listen` opens a socket as `tag` and
-- | returns the closer; `fetch` hands raw HTTP (a WebSocket upgrade, or a
-- | request for the object's `fetch` hook) to the object, Worker side only.
type Transport =
  { call :: Id -> RawCall
  , unique :: Aff Id
  , listen :: Id -> String -> Listener -> Effect (Effect Unit)
  , fetch :: Id -> Request -> Aff Response
  }

-- | An object's contract paired with a way to reach its instances. Every
-- | typed operation below is the untyped transport seen through the contract.
newtype Namespace :: Symbol -> Row Type -> Row Type -> Type
newtype Namespace name api events = Namespace
  { object :: Object name api events
  , transport :: Transport
  }

namespace :: forall name api events. Object name api events -> Transport -> Namespace name api events
namespace o t = Namespace { object: o, transport: t }

get :: forall name api events. Namespace name api events -> ObjectId name -> Record api
get (Namespace { object: Object o, transport }) (ObjectId id) = o.connect (transport.call id)

getByName :: forall name api events. Namespace name api events -> String -> Record api
getByName ns = get ns <<< ObjectId <<< Named

-- | Subscribe to an object's events as `tag`. Returns the unsubscribe. The
-- | shape is `makeEmitter`'s, so `makeEmitter (listen ns id tag)` is an `Emitter`.
listen
  :: forall name api events
   . Namespace name api events
  -> ObjectId name
  -> String
  -> (Signal (Variant events) -> Effect Unit)
  -> Effect (Effect Unit)
listen (Namespace { object: Object o, transport }) (ObjectId id) tag deliver =
  transport.listen id tag $ deliver <<< decoded o.decodeEvent

-- | Plain HTTP into an object's `fetch` hook. Worker side and simulator;
-- | a browser reaches it through `Http.route` at `.../http/<path>`.
http :: forall name api events. Namespace name api events -> ObjectId name -> Request -> Aff Response
http (Namespace { transport }) (ObjectId id) = transport.fetch id

newUniqueId :: forall name api events. Namespace name api events -> Aff (ObjectId name)
newUniqueId (Namespace { transport }) = ObjectId <$> transport.unique

idFromName :: forall name api events. Namespace name api events -> String -> ObjectId name
idFromName _ = ObjectId <<< Named

idFromString :: forall name api events. Namespace name api events -> String -> ObjectId name
idFromString _ = ObjectId <<< Unique

idToString :: forall name. ObjectId name -> String
idToString (ObjectId id) = printId id
