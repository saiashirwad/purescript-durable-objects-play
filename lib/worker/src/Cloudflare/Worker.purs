module Cloudflare.Worker
  ( ContainerSpec
  , ObjectBinding
  , Plan
  , Request
  , Response
  , Route
  , Worker
  , WorkerInit
  , WorkerRef
  , body
  , bodyBase64
  , bytes
  , cookie
  , guard
  , header
  , json
  , make
  , method
  , objectBinding
  , pathname
  , plan
  , protect
  , rebase
  , ref
  , requestTo
  , requestWith
  , responseText
  , responseHeader
  , status
  , route
  , scriptName
  , serve
  , sha256
  , text
  , textWith
  , toExport
  , url
  , variable
  , wranglerConfig
  ) where

import Prelude

import Cloudflare.Static (Static, static)
import Cloudflare.Static as Static
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Plus (empty)
import Control.Apply (lift2)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Int as Int
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Foreign.Object as Object

foreign import data Request :: Type
foreign import data Response :: Type
foreign import url :: Request -> String
foreign import method :: Request -> String
foreign import pathname :: Request -> String
foreign import text :: Int -> String -> Response
foreign import json :: Int -> Json -> Response
foreign import bodyImpl :: Request -> Effect (Promise Json)
foreign import headerImpl :: Request -> String -> Nullable String
foreign import rebase :: String -> Request -> Request
foreign import cookieImpl :: Request -> String -> Nullable String
foreign import textWith :: Int -> Array { name :: String, value :: String } -> String -> Response
foreign import sha256Impl :: String -> Effect (Promise String)
foreign import requestTo :: String -> Request
foreign import requestWith :: { url :: String, method :: String, contentType :: String, base64 :: String } -> Request
foreign import bodyBase64Impl :: Request -> Effect (Promise String)
foreign import bytes :: Int -> String -> String -> Response
foreign import responseHeaderImpl :: Response -> String -> Nullable String
foreign import status :: Response -> Int
foreign import responseTextImpl :: Response -> Effect (Promise String)
foreign import variableImpl :: Foreign -> String -> Effect String
foreign import bindingImpl :: Foreign -> String -> Effect Foreign
foreign import toExportImpl :: (Foreign -> Effect { fetch :: Request -> Effect (Promise Response) }) -> Foreign

type ContainerSpec = { image :: String, instances :: Int, instanceType :: String }

type ObjectBinding = { className :: String, binding :: String, scriptName :: Maybe String, container :: Maybe ContainerSpec }

type Plan = { objects :: Array ObjectBinding, variables :: Array String }

type WorkerInit = Static Plan Foreign Effect

variable :: String -> WorkerInit String
variable name = static { objects: [], variables: [ name ] } \env -> variableImpl env name

objectBinding :: ObjectBinding -> WorkerInit Foreign
objectBinding b = static { objects: [ b ], variables: [] } \env -> bindingImpl env b.binding

body :: Request -> Aff Json
body = toAffE <<< bodyImpl

responseText :: Response -> Aff String
responseText = toAffE <<< responseTextImpl

responseHeader :: Response -> String -> Maybe String
responseHeader response = toMaybe <<< responseHeaderImpl response

-- | The request body, base64.
bodyBase64 :: Request -> Aff String
bodyBase64 = toAffE <<< bodyBase64Impl

header :: Request -> String -> Maybe String
header request = toMaybe <<< headerImpl request

cookie :: Request -> String -> Maybe String
cookie request = toMaybe <<< cookieImpl request

sha256 :: String -> Aff String
sha256 = toAffE <<< sha256Impl

newtype WorkerRef = WorkerRef String

ref :: String -> WorkerRef
ref = WorkerRef

scriptName :: WorkerRef -> String
scriptName (WorkerRef name) = name

-- | Routes that know what they need bound. `a <> b` binds what both bind
-- | and tries `a`'s routes first; the monoid is `Route`'s, lifted through
-- | `WorkerInit`.
newtype Worker = Worker (WorkerInit Route)

derive newtype instance semigroupWorker :: Semigroup Worker
derive newtype instance monoidWorker :: Monoid Worker

make :: WorkerInit Route -> Worker
make = Worker

-- | `a <> b` tries `a`, then `b`. `mempty` matches nothing.
newtype Route = Route (Request -> MaybeT Aff Response)

instance semigroupRoute :: Semigroup Route where
  append (Route f) (Route g) = Route \request -> f request <|> g request

instance monoidRoute :: Monoid Route where
  mempty = Route \_ -> empty

route :: (Request -> Aff (Maybe Response)) -> Route
route handler = Route $ MaybeT <<< handler

-- | Let requests through only when `allowed` says so; 401 otherwise.
guard :: (Request -> Aff Boolean) -> Route -> Route
guard allowed (Route handler) = Route \request -> MaybeT do
  ok <- allowed request
  if ok then runMaybeT (handler request) else pure $ Just $ text 401 "passkey required"

-- | `guard`, over a whole Worker: the check may need bindings of its own.
protect :: WorkerInit (Request -> Aff Boolean) -> Worker -> Worker
protect check (Worker routes) = Worker $ lift2 guard check routes

-- | 404 when nothing matches.
serve :: Route -> Request -> Aff Response
serve (Route handler) request = runMaybeT (handler request) <#> case _ of
  Just response -> response
  Nothing -> text 404 "not found"

plan :: Worker -> Plan
plan (Worker w) = Static.plan w

toExport :: Worker -> Foreign
toExport (Worker w) = toExportImpl \env -> do
  routes <- Static.build w env
  pure { fetch: fromAff <<< serve routes }

wranglerConfig
  :: { name :: String, main :: String, compatibilityDate :: String, assets :: Maybe String, containers :: Boolean }
  -> Worker
  -> Json
wranglerConfig options worker = J.fromObject $ Object.fromFoldable $
  [ "name" /\ J.fromString options.name
  , "main" /\ J.fromString options.main
  , "compatibility_date" /\ J.fromString options.compatibilityDate
  , "durable_objects" /\ J.fromObject (Object.singleton "bindings" (J.fromArray (bindingJson <$> objects)))
  , "exports" /\ J.fromObject (Object.fromFoldable (exportJson <$> hosted))
  ] <> Array.catMaybes
    [ "containers" /\ J.fromArray (containerJson <$> containers) <$ Array.head containers
    , options.assets <#> \directory -> "assets" /\ J.fromObject (Object.singleton "directory" (J.fromString directory))
    ]
  where
  objects = Array.nubEq (plan worker).objects
  hosted = objects # Array.filter (\o -> o.scriptName == Nothing)

  containers = if options.containers then hosted # Array.mapMaybe \o -> { className: o.className, spec: _ } <$> o.container else []

  containerJson { className, spec } = J.fromObject $ Object.fromFoldable
    [ "class_name" /\ J.fromString className
    , "image" /\ J.fromString spec.image
    , "max_instances" /\ J.fromNumber (Int.toNumber spec.instances)
    , "instance_type" /\ J.fromString spec.instanceType
    ]

  bindingJson o = J.fromObject $ Object.fromFoldable $
    [ "name" /\ J.fromString o.binding, "class_name" /\ J.fromString o.className ]
      <> case o.scriptName of
        Just script -> [ "script_name" /\ J.fromString script ]
        Nothing -> []

  exportJson o = o.className /\ J.fromObject
    (Object.fromFoldable [ "type" /\ J.fromString "durable-object", "storage" /\ J.fromString "sqlite" ])
