module Cloudflare.Durable.Protocol
  ( RawCall
  , RawHandler
  , class Connect
  , class MethodNames
  , class Serve
  , connect
  , connectList
  , methodNames
  , envelopeCodec
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
import Data.Bifunctor (lmap)
import Data.Codec (codec')
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff, attempt, message)
import Prim.Row as Row
import Prim.RowList (Cons, Nil, RowList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

type RawCall = String -> Json -> Aff Json

type RawHandler = Json -> Aff Json

class MethodNames (list :: RowList Type) where
  methodNames :: Proxy list -> Array String

instance methodNamesNil :: MethodNames Nil where
  methodNames _ = []

instance methodNamesCons :: (IsSymbol name, MethodNames tail) => MethodNames (Cons name value tail) where
  methodNames _ = [ reflectSymbol (Proxy :: Proxy name) ] <> methodNames (Proxy :: Proxy tail)

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
  CA.encode (envelopeCodec m.error m.success) <$> case CA.decode m.request request of
    Left err -> pure $ Left $ decodeFailure err
    Right value -> join <<< lmap (RemoteDefect <<< message) <$> attempt (Rpc.run (implementation value))

stub :: forall e req res. String -> Method e req res -> RawCall -> req -> Rpc e res
stub name (Method m) call request = Rpc $ ExceptT $
  attempt (call name (CA.encode m.request request)) <#> case _ of
    Left exception -> Left $ TransportError $ message exception
    Right response -> join $ lmap decodeFailure $ CA.decode (envelopeCodec m.error m.success) response

decodeFailure :: forall e. JsonDecodeError -> RpcFailure e
decodeFailure = DecodeError <<< CA.printJsonDecodeError

-- | `{ "tag": ..., ... }` on the wire: `ok` carries `value`, `error` carries
-- | `error`, `platform` carries `operation` and `message`, the rest `message`.
envelopeCodec :: forall e a. JsonCodec e -> JsonCodec a -> JsonCodec (Either (RpcFailure e) a)
envelopeCodec errorCodec successCodec = codec' decode encode
  where
  tagOnly = CAR.object "envelope" { tag: CA.string }
  ok = CAR.object "ok" { tag: CA.string, value: successCodec }
  domain = CAR.object "error" { tag: CA.string, error: errorCodec }
  platform = CAR.object "platform" { tag: CA.string, operation: CA.string, message: CA.string }
  said = CAR.object "message" { tag: CA.string, message: CA.string }

  encode = case _ of
    Right value -> CA.encode ok { tag: "ok", value }
    Left (DomainError error) -> CA.encode domain { tag: "error", error }
    Left (PlatformError (Runtime.PlatformError { operation, message })) -> CA.encode platform { tag: "platform", operation, message }
    Left (TransportError message) -> CA.encode said { tag: "transport", message }
    Left (DecodeError message) -> CA.encode said { tag: "decode", message }
    Left (RemoteDefect message) -> CA.encode said { tag: "defect", message }

  decode json = CA.decode tagOnly json >>= \{ tag } -> case tag of
    "ok" -> Right <<< _.value <$> CA.decode ok json
    "error" -> Left <<< DomainError <<< _.error <$> CA.decode domain json
    "platform" -> CA.decode platform json <#> \{ operation, message } -> Left $ PlatformError $ Runtime.PlatformError { operation, message }
    "transport" -> Left <<< TransportError <<< _.message <$> CA.decode said json
    "decode" -> Left <<< DecodeError <<< _.message <$> CA.decode said json
    "defect" -> Left <<< RemoteDefect <<< _.message <$> CA.decode said json
    other -> Left $ CA.Named "envelope tag" $ CA.UnexpectedValue $ J.fromString other
