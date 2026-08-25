-- | The wire between a client stub and an object. Both directions are derived
-- | from the descriptor record by walking its row, so no stub is generated and
-- | no method is inspected at runtime.
module Cloudflare.Durable.Protocol
  ( RawCall
  , RawHandler
  , class Connect
  , class MethodNames
  , class Serve
  , connect
  , connectList
  , methodNames
  , decodeEnvelope
  , encodeEnvelope
  , serve
  , serveList
  ) where

import Prelude

import Cloudflare.Durable.Rpc (Method(..), Rpc(..), RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Runtime as Runtime
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, attempt, message)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (RowList, Cons, Nil)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | Sends one encoded request to a named method and returns the encoded envelope.
type RawCall = String -> Json -> Aff Json

-- | Handles one encoded request and returns the encoded envelope.
type RawHandler = Json -> Aff Json

-- | The method names in a descriptor row.
class MethodNames (list :: RowList Type) where
  methodNames :: Proxy list -> Array String

instance methodNamesNil :: MethodNames Nil where
  methodNames _ = []

instance methodNamesCons :: (IsSymbol name, MethodNames tail) => MethodNames (Cons name value tail) where
  methodNames _ = [ reflectSymbol (Proxy :: Proxy name) ] <> methodNames (Proxy :: Proxy tail)

-- | Build the server-side dispatch table from a descriptor and an implementation.
class Serve (list :: RowList Type) (spec :: Row Type) (api :: Row Type) where
  serveList :: Proxy list -> Record spec -> Record api -> Map String RawHandler

instance serveNil :: Serve Nil spec api where
  serveList _ _ _ = Map.empty

instance serveCons ::
  ( IsSymbol name
  , Row.Cons name (Method e req res) specTail spec
  , Row.Cons name (req -> Rpc e res) apiTail api
  , Serve tail spec api
  ) =>
  Serve (Cons name (Method e req res) tail) spec api where
  serveList _ spec impl =
    Map.insert (reflectSymbol name) (handler (Record.get name spec) (Record.get name impl))
      $ serveList (Proxy :: Proxy tail) spec impl
    where
    name = Proxy :: Proxy name

-- | Build the client stub from a descriptor and a transport.
class Connect (list :: RowList Type) (spec :: Row Type) (api :: Row Type) | list -> api where
  connectList :: Proxy list -> Record spec -> RawCall -> Builder (Record ()) (Record api)

instance connectNil :: Connect Nil spec () where
  connectList _ _ _ = identity

instance connectCons ::
  ( IsSymbol name
  , Row.Cons name (Method e req res) specTail spec
  , Row.Cons name (req -> Rpc e res) apiTail api
  , Row.Lacks name apiTail
  , Connect tail spec apiTail
  ) =>
  Connect (Cons name (Method e req res) tail) spec api where
  connectList _ spec call =
    Builder.insert name (stub (reflectSymbol name) (Record.get name spec) call)
      <<< connectList (Proxy :: Proxy tail) spec call
    where
    name = Proxy :: Proxy name

serve :: forall list spec api. Serve list spec api => Proxy list -> Record spec -> Record api -> Map String RawHandler
serve = serveList

connect :: forall list spec api. Connect list spec api => Proxy list -> Record spec -> RawCall -> Record api
connect list spec call = Builder.build (connectList list spec call) {}

handler :: forall e req res. Method e req res -> (req -> Rpc e res) -> RawHandler
handler (Method m) implementation request =
  encodeEnvelope m.error m.success <$> case CA.decode m.request request of
    Left err -> pure $ Left $ DecodeError $ CA.printJsonDecodeError err
    Right value -> attempt (Rpc.run (implementation value)) <#> case _ of
      Left exception -> Left $ RemoteDefect $ message exception
      Right result -> result

stub :: forall e req res. String -> Method e req res -> RawCall -> req -> Rpc e res
stub name (Method m) call request = Rpc $ ExceptT do
  response <- attempt $ call name (CA.encode m.request request)
  pure case response of
    Left exception -> Left $ TransportError $ message exception
    Right envelope -> case decodeEnvelope m.error m.success envelope of
      Left err -> Left $ DecodeError $ CA.printJsonDecodeError err
      Right result -> result

-- | The response envelope: one tag per outcome.
encodeEnvelope :: forall e a. JsonCodec e -> JsonCodec a -> Either (RpcFailure e) a -> Json
encodeEnvelope errorCodec successCodec = case _ of
  Right value -> tagged "ok" [ "value" /\ CA.encode successCodec value ]
  Left (DomainError e) -> tagged "error" [ "error" /\ CA.encode errorCodec e ]
  Left (PlatformError (Runtime.PlatformError { operation, message })) ->
    tagged "platform" [ "operation" /\ J.fromString operation, "message" /\ J.fromString message ]
  Left (TransportError m) -> tagged "transport" [ "message" /\ J.fromString m ]
  Left (DecodeError m) -> tagged "decode" [ "message" /\ J.fromString m ]
  Left (RemoteDefect m) -> tagged "defect" [ "message" /\ J.fromString m ]
  where
  tagged tag fields = J.fromObject $ Object.fromFoldable $ [ "tag" /\ J.fromString tag ] <> fields

decodeEnvelope :: forall e a. JsonCodec e -> JsonCodec a -> Json -> Either CA.JsonDecodeError (Either (RpcFailure e) a)
decodeEnvelope errorCodec successCodec json = do
  fields <- note (CA.TypeMismatch "Object") $ J.toObject json
  let
    field key = note (CA.AtKey key CA.MissingValue) $ Object.lookup key fields
    text key = field key >>= J.toString >>> note (CA.AtKey key (CA.TypeMismatch "String"))
  tag <- text "tag"
  case tag of
    "ok" -> Right <$> (field "value" >>= CA.decode successCodec)
    "error" -> Left <<< DomainError <$> (field "error" >>= CA.decode errorCodec)
    "platform" -> do
      operation <- text "operation"
      message <- text "message"
      pure $ Left $ PlatformError $ Runtime.PlatformError { operation, message }
    "transport" -> Left <<< TransportError <$> text "message"
    "decode" -> Left <<< DecodeError <$> text "message"
    "defect" -> Left <<< RemoteDefect <$> text "message"
    other -> Left $ CA.Named "envelope tag" $ CA.UnexpectedValue $ J.fromString other
