-- | Events an object pushes to its sockets, typed as a row: `Variant events`
-- | on both ends, `{ "event": tag, "value": json }` on the wire.
module Cloudflare.Durable.Events
  ( Event
  , Signal(..)
  , class DecodeEvents
  , class EncodeEvents
  , decodeEvents
  , encodeEvents
  , event
  , eventWith
  , unwire
  , wire
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec, codec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, case_, inj, on)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (RowList, Cons, Nil)
import Record as Record
import Type.Proxy (Proxy(..))

newtype Event a = Event (JsonCodec a)

event :: forall a. HasCodec a => Event a
event = eventWith codec

eventWith :: forall a. JsonCodec a -> Event a
eventWith = Event

-- | What a listener sees: the connection opening and closing, each decoded
-- | event, and anything that failed to decode.
data Signal a
  = Opened
  | Closed
  | Delivered a
  | Garbled String

derive instance functorSignal :: Functor Signal
derive instance eqSignal :: Eq a => Eq (Signal a)

instance showSignal :: Show a => Show (Signal a) where
  show = case _ of
    Opened -> "Opened"
    Closed -> "Closed"
    Delivered a -> "(Delivered " <> show a <> ")"
    Garbled m -> "(Garbled " <> show m <> ")"

class EncodeEvents (list :: RowList Type) (spec :: Row Type) (events :: Row Type) | list -> events where
  encodeEvents :: Proxy list -> Record spec -> Variant events -> Json

instance encodeEventsNil :: EncodeEvents Nil spec () where
  encodeEvents _ _ = case_

instance encodeEventsCons ::
  ( IsSymbol name
  , Row.Cons name (Event a) specTail spec
  , Row.Cons name a eventsTail events
  , EncodeEvents tail spec eventsTail
  ) =>
  EncodeEvents (Cons name (Event a) tail) spec events where
  encodeEvents _ spec = on name (\value -> wire (reflectSymbol name) (CA.encode c value)) (encodeEvents (Proxy :: Proxy tail) spec)
    where
    name = Proxy :: Proxy name
    Event c = Record.get name spec

class DecodeEvents (list :: RowList Type) (spec :: Row Type) (events :: Row Type) where
  decodeEvents :: Proxy list -> Record spec -> String -> Json -> Maybe (Either JsonDecodeError (Variant events))

instance decodeEventsNil :: DecodeEvents Nil spec events where
  decodeEvents _ _ _ _ = Nothing

instance decodeEventsCons ::
  ( IsSymbol name
  , Row.Cons name (Event a) specTail spec
  , Row.Cons name a rest events
  , DecodeEvents tail spec events
  ) =>
  DecodeEvents (Cons name (Event a) tail) spec events where
  decodeEvents _ spec tag json =
    if tag == reflectSymbol name then Just $ inj name <$> CA.decode c json
    else decodeEvents (Proxy :: Proxy tail) spec tag json
    where
    name = Proxy :: Proxy name
    Event c = Record.get name spec

wire :: String -> Json -> Json
wire tag value = J.fromObject $ Object.fromFoldable [ "event" /\ J.fromString tag, "value" /\ value ]

unwire :: Json -> Either JsonDecodeError (Tuple String Json)
unwire json = do
  fields <- note (TypeMismatch "Object") $ J.toObject json
  tag <- note (AtKey "event" MissingValue) (Object.lookup "event" fields) >>= J.toString >>> note (AtKey "event" (TypeMismatch "String"))
  value <- note (AtKey "value" MissingValue) $ Object.lookup "value" fields
  pure $ Tuple tag value
