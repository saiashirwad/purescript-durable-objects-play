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
  , emitting
  , get
  , getByName
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
  , sockets
  ) where

import Prelude

import Cloudflare.Durable.Events (class DecodeEvents, class EncodeEvents, Signal(..), decodeEvents, encodeEvents, unwire)
import Cloudflare.Durable.Init (Env, Init)
import Cloudflare.Durable.Init as Init
import Cloudflare.Durable.Protocol (class Connect, class MethodNames, class Serve, RawCall, RawHandler, connect, methodNames, serve)
import Cloudflare.Durable.Runtime (Runtime, Socket)
import Cloudflare.Durable.Runtime as Runtime
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Static (static)
import Cloudflare.Worker (Request, Response)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

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
emitting (Object o) spec = Object
  { name: o.name
  , methods: o.methods
  , serve: o.serve
  , connect: o.connect
  , encodeEvent: encodeEvents list spec
  , decodeEvent: \json -> do
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

-- | What activation yields. `alarm` runs when a scheduled alarm is due;
-- | `connect` and `disconnect` run as sockets come and go. Each is a monoid
-- | whose `mempty` does nothing.
type Handlers api =
  { methods :: Record api
  , alarm :: Runtime Unit
  , connect :: Socket -> Runtime Unit
  , disconnect :: Socket -> Runtime Unit
  }

newtype Live :: Symbol -> Row Type -> Row Type -> Type
newtype Live name api events = Live
  { object :: Object name api events
  , activate :: Init (Runtime (Handlers api))
  }

implement :: forall name api events. Object name api events -> Init (Runtime (Record api)) -> Live name api events
implement o = implementWith o <<< map (map { methods: _, alarm: mempty, connect: mempty, disconnect: mempty })

implementWith :: forall name api events. Object name api events -> Init (Runtime (Handlers api)) -> Live name api events
implementWith o activation = Live { object: o, activate: activation }

-- | The object's sockets, typed by its event row. Use during `Init` like `state`.
sockets :: forall name api events. Object name api events -> Init (Sockets (Variant events))
sockets (Object o) = static mempty \env -> pure $ Sockets.fromRaw o.encodeEvent env.sockets

type Manifest = { className :: String, methods :: Array String, variables :: Array String }

manifest :: forall name api events. Live name api events -> Manifest
manifest (Live { object: Object o, activate: activation }) =
  { className: o.name, methods: o.methods, variables: (Init.plan activation).variables }

type Activated =
  { methods :: Map String RawHandler
  , alarm :: Aff Unit
  , connect :: Socket -> Aff Unit
  , disconnect :: Socket -> Aff Unit
  }

-- | Hooks rethrow a `PlatformError` as an exception so the platform sees it
-- | (and retries an alarm).
activate :: forall name api events. Live name api events -> Env -> Aff Activated
activate (Live { object: Object o, activate: activation }) env = do
  outcome <- Runtime.run $ join $ Init.build activation env
  case outcome of
    Right handlers -> pure
      { methods: o.serve handlers.methods
      , alarm: rethrow handlers.alarm
      , connect: rethrow <<< handlers.connect
      , disconnect: rethrow <<< handlers.disconnect
      }
    Left failure -> throwError $ error $ o.name <> " failed to activate: " <> show failure
  where
  rethrow action = Runtime.run action >>= either (throwError <<< error <<< show) pure

data Id
  = Named String
  | Unique String

derive instance eqId :: Eq Id
derive instance ordId :: Ord Id

instance showId :: Show Id where
  show (Named name) = "(Named " <> show name <> ")"
  show (Unique id) = "(Unique " <> show id <> ")"

newtype ObjectId :: Symbol -> Type
newtype ObjectId name = ObjectId Id

derive newtype instance eqObjectId :: Eq (ObjectId name)
derive newtype instance ordObjectId :: Ord (ObjectId name)
derive newtype instance showObjectId :: Show (ObjectId name)

type Listener = Signal Json -> Effect Unit

-- | How a namespace reaches its objects. `listen` opens a socket as `tag` and
-- | returns the closer; `upgrade` hands a WebSocket upgrade request to the
-- | object (Worker side only).
type Transport =
  { call :: Id -> RawCall
  , unique :: Aff Id
  , listen :: Id -> String -> Listener -> Effect (Effect Unit)
  , upgrade :: Id -> Request -> Aff Response
  }

newtype Namespace :: Symbol -> Row Type -> Row Type -> Type
newtype Namespace name api events = Namespace
  { name :: String
  , stub :: Id -> Record api
  , call :: Id -> RawCall
  , unique :: Aff Id
  , listen :: Id -> String -> Listener -> Effect (Effect Unit)
  , upgrade :: Id -> Request -> Aff Response
  , decodeEvent :: Json -> Either JsonDecodeError (Variant events)
  }

namespace :: forall name api events. Object name api events -> Transport -> Namespace name api events
namespace (Object o) transport = Namespace
  { name: o.name
  , stub: o.connect <<< transport.call
  , call: transport.call
  , unique: transport.unique
  , listen: transport.listen
  , upgrade: transport.upgrade
  , decodeEvent: o.decodeEvent
  }

getByName :: forall name api events. Namespace name api events -> String -> Record api
getByName (Namespace ns) = ns.stub <<< Named

get :: forall name api events. Namespace name api events -> ObjectId name -> Record api
get (Namespace ns) (ObjectId id) = ns.stub id

-- | Subscribe to an object's events as `tag`. Returns the unsubscribe. The
-- | shape is `makeEmitter`'s, so `makeEmitter (listen ns id tag)` is an `Emitter`.
listen
  :: forall name api events
   . Namespace name api events
  -> ObjectId name
  -> String
  -> (Signal (Variant events) -> Effect Unit)
  -> Effect (Effect Unit)
listen (Namespace ns) (ObjectId id) tag deliver = ns.listen id tag $ deliver <<< case _ of
  Delivered json -> either (Garbled <<< printJsonDecodeError) Delivered $ ns.decodeEvent json
  Opened -> Opened
  Closed -> Closed
  Garbled m -> Garbled m

idFromName :: forall name api events. Namespace name api events -> String -> ObjectId name
idFromName _ = ObjectId <<< Named

newUniqueId :: forall name api events. Namespace name api events -> Aff (ObjectId name)
newUniqueId (Namespace ns) = ObjectId <$> ns.unique

-- | Unique ids print as hex; named ids print as their name.
idToString :: forall name. ObjectId name -> String
idToString (ObjectId id) = case id of
  Unique hex -> hex
  Named name -> name

idFromString :: forall name api events. Namespace name api events -> String -> ObjectId name
idFromString _ = ObjectId <<< Unique
