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
  , loopback
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
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff, error, throwError)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

newtype Object :: Symbol -> Row Type -> Type
newtype Object name api = Object
  { name :: String
  , methods :: Array String
  , serve :: Record api -> Map String RawHandler
  , connect :: RawCall -> Record api
  }

-- | The class name comes from the signature, `counter :: Object "Counter" CounterApi`.
-- | Each `method` field takes its types from the API row.
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

-- | `connect` after `serve`: an implementation seen through the wire. The
-- | simulator is this, one per id; the tests check it behaves as the
-- | implementation.
loopback :: forall name api. Object name api -> Record api -> Record api
loopback (Object o) impl = o.connect \name request ->
  case Map.lookup name (o.serve impl) of
    Just handle -> handle request
    Nothing -> throwError $ error $ o.name <> " has no method " <> show name

type Handlers api = { methods :: Record api, alarm :: Maybe (Runtime Unit) }

newtype Live :: Symbol -> Row Type -> Type
newtype Live name api = Live
  { object :: Object name api
  , activate :: Init (Runtime (Handlers api))
  }

implement :: forall name api. Object name api -> Init (Runtime (Record api)) -> Live name api
implement o = implementWith o <<< map (map { methods: _, alarm: Nothing })

implementWith :: forall name api. Object name api -> Init (Runtime (Handlers api)) -> Live name api
implementWith o activation = Live { object: o, activate: activation }

type Manifest = { className :: String, methods :: Array String, variables :: Array String }

manifest :: forall name api. Live name api -> Manifest
manifest (Live { object: Object o, activate: activation }) =
  { className: o.name, methods: o.methods, variables: (Init.plan activation).variables }

activate :: forall name api. Live name api -> Env -> Aff (Map String RawHandler)
activate (Live { object: Object o, activate: activation }) env = do
  outcome <- Runtime.run $ join $ Init.build activation env
  case outcome of
    Right handlers -> pure $ o.serve handlers.methods
    Left failure -> throwError $ error $ o.name <> " failed to activate: " <> show failure

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

type Transport = { call :: Id -> RawCall, unique :: Aff Id }

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

newUniqueId :: forall name api. Namespace name api -> Aff (ObjectId name)
newUniqueId (Namespace ns) = ObjectId <$> ns.unique

-- | Unique ids print as hex; named ids print as their name.
idToString :: forall name. ObjectId name -> String
idToString (ObjectId id) = case id of
  Unique hex -> hex
  Named name -> name

idFromString :: forall name api. Namespace name api -> String -> ObjectId name
idFromString _ = ObjectId <<< Unique
