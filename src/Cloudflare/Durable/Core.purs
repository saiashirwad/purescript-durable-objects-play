-- | The core types with their constructors. Backends (the simulator, the
-- | Worker bridge) import this; applications import `Cloudflare.Durable`.
module Cloudflare.Durable.Core
  ( Handlers
  , Id(..)
  , Live(..)
  , Manifest
  , Namespace(..)
  , Object(..)
  , ObjectId(..)
  , Transport
  , activate
  , className
  , get
  , getByName
  , idFromName
  , idFromString
  , idToString
  , implement
  , implementWith
  , manifest
  , namespace
  , newUniqueId
  , object
  ) where

import Prelude

import Cloudflare.Durable.Init (Env, Init)
import Cloudflare.Durable.Init as Init
import Cloudflare.Durable.Protocol (class Connect, class MethodNames, class Serve, RawCall, RawHandler, connect, methodNames, serve)
import Cloudflare.Durable.Runtime (Runtime)
import Cloudflare.Durable.Runtime as Runtime
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff, error, throwError)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

-- | An object's identity and contract: its class name and its API row.
newtype Object :: Symbol -> Row Type -> Type
newtype Object name api = Object
  { name :: String
  , methods :: Array String
  , serve :: Record api -> Map String RawHandler
  , connect :: RawCall -> Record api
  }

-- | Declare an object from its descriptor record. The class name comes from
-- | the type, so write the signature:
-- |
-- | ```purescript
-- | counter :: Object "Counter" CounterApi
-- | counter = object { increment: method, get: method }
-- | ```
-- |
-- | The compiler derives `CounterApi` from the descriptor and checks any
-- | annotation against it. `method` is polymorphic; the annotation fixes its
-- | request, response, and error types, and `HasCodec` supplies the codecs.
object
  :: forall name spec list api
   . IsSymbol name
  => RowToList spec list
  => MethodNames list
  => Connect list spec api
  => Serve list spec api
  => Record spec
  -> Object name api
object spec = Object
  { name: reflectSymbol (Proxy :: Proxy name)
  , methods: methodNames list
  , serve: serve list spec
  , connect: connect list spec
  }
  where
  list = Proxy :: Proxy list

className :: forall name api. Object name api -> String
className (Object o) = o.name

-- | Everything an object answers to. `methods` is the RPC record; `alarm`
-- | runs when a scheduled alarm fires.
type Handlers api = { methods :: Record api, alarm :: Maybe (Runtime Unit) }

-- | An object together with its implementation.
newtype Live :: Symbol -> Row Type -> Type
newtype Live name api = Live
  { object :: Object name api
  , activate :: Init (Runtime (Handlers api))
  }

-- | Give an object its implementation. The outer `Init` gathers what the
-- | object needs; the inner `Runtime` runs once per activation and returns
-- | the method record.
implement :: forall name api. Object name api -> Init (Runtime (Record api)) -> Live name api
implement o = implementWith o <<< map (map { methods: _, alarm: Nothing })

implementWith :: forall name api. Object name api -> Init (Runtime (Handlers api)) -> Live name api
implementWith o activation = Live { object: o, activate: activation }

-- | What a deployment must provide for a `Live` object.
type Manifest = { className :: String, methods :: Array String, variables :: Array String }

manifest :: forall name api. Live name api -> Manifest
manifest (Live { object: Object o, activate: activation }) =
  { className: o.name, methods: o.methods, variables: (Init.plan activation).variables }

-- | Activate one instance: run `Init` against its environment, then the
-- | activation, and return the dispatch table. A failed activation is a
-- | defect; the instance cannot serve.
activate :: forall name api. Live name api -> Env -> Aff (Map String RawHandler)
activate (Live { object: Object o, activate: activation }) env = do
  outcome <- Runtime.run $ join $ Init.build activation env
  case outcome of
    Right handlers -> pure $ o.serve handlers.methods
    Left failure -> throwError $ error $ o.name <> " failed to activate: " <> show failure

-- | How an instance is addressed.
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

-- | What a backend gives a `Namespace`: a way to call any instance, and a
-- | way to mint ids.
type Transport = { call :: Id -> RawCall, unique :: Aff Id }

-- | A handle for addressing instances of one object class.
newtype Namespace :: Symbol -> Row Type -> Type
newtype Namespace name api = Namespace
  { name :: String
  , stub :: Id -> Record api
  , call :: Id -> RawCall
  , unique :: Aff Id
  }

namespace :: forall name api. Object name api -> Transport -> Namespace name api
namespace (Object o) transport = Namespace
  { name: o.name
  , stub: o.connect <<< transport.call
  , call: transport.call
  , unique: transport.unique
  }

getByName :: forall name api. Namespace name api -> String -> Record api
getByName (Namespace ns) = ns.stub <<< Named

get :: forall name api. Namespace name api -> ObjectId name -> Record api
get (Namespace ns) (ObjectId id) = ns.stub id

idFromName :: forall name api. Namespace name api -> String -> ObjectId name
idFromName _ = ObjectId <<< Named

-- | Mint an id no name maps to. Its string form is the thing to put in a
-- | link.
newUniqueId :: forall name api. Namespace name api -> Aff (ObjectId name)
newUniqueId (Namespace ns) = ObjectId <$> ns.unique

-- | The string form of an id: the hex of a unique id, or the name of a
-- | named one.
idToString :: forall name. ObjectId name -> String
idToString (ObjectId id) = case id of
  Unique hex -> hex
  Named name -> name

-- | Read back a unique id printed by `idToString`. For named ids use
-- | `idFromName`.
idFromString :: forall name api. Namespace name api -> String -> ObjectId name
idFromString _ = ObjectId <<< Unique
