-- | Events an object pushes to its sockets, typed as a row: `Variant events`
-- | on both ends, `{ "event": tag, "value": json }` on the wire.
-- |
-- | `variantCodec` exposes both directions as one
-- | `JsonCodec (Variant events)`.
module Cloudflare.Durable.Events
  ( Event
  , Signal(..)
  , class DecodeEvents
  , class EncodeEvents
  , decodeEvents
  , decoded
  , encodeEvents
  , event
  , eventWith
  , variantCodec
  , wireCodec
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec, codec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec (codec')
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, case_, inj, on)
import Prim.Row as Row
import Prim.RowList (Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))

newtype Event a = Event (JsonCodec a)

event :: forall a. HasCodec a => Event a
event = eventWith codec

eventWith :: forall a. JsonCodec a -> Event a
eventWith = Event

-- | What a listener sees: the connection opening and closing, each decoded
-- | event, and anything that failed to decode.
-- |
-- | A monad in the shape of `Maybe`, with reasons: `Delivered` is `pure`,
-- | the other three end a `bind` early.
data Signal a
  = Opened
  | Closed
  | Delivered a
  | Garbled String

derive instance functorSignal :: Functor Signal
derive instance eqSignal :: Eq a => Eq (Signal a)
derive instance genericSignal :: Generic (Signal a) _

instance showSignal :: Show a => Show (Signal a) where
  show = genericShow

instance applySignal :: Apply Signal where
  apply = ap

instance applicativeSignal :: Applicative Signal where
  pure = Delivered

instance bindSignal :: Bind Signal where
  bind = case _ of
    Delivered a -> \k -> k a
    Opened -> const Opened
    Closed -> const Closed
    Garbled why -> const (Garbled why)

instance monadSignal :: Monad Signal

-- | Decode what was delivered; a failure is `Garbled`.
decoded :: forall a. (Json -> Either JsonDecodeError a) -> Signal Json -> Signal a
decoded decode = (_ >>= either (Garbled <<< printJsonDecodeError) Delivered <<< decode)

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
  encodeEvents _ spec =
    on name (\value -> CA.encode wireCodec { event: reflectSymbol name, value: CA.encode c value }) (encodeEvents (Proxy :: Proxy tail) spec)
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

-- | Both directions as one codec: `{ "event": tag, "value": json }` on the
-- | wire. Decoding fails under `Named "event"`, with `UnexpectedValue` for a
-- | tag no event owns.
variantCodec
  :: forall list spec events
   . EncodeEvents list spec events
  => DecodeEvents list spec events
  => Proxy list
  -> Record spec
  -> JsonCodec (Variant events)
variantCodec list spec = codec' decode encode
  where
  encode = encodeEvents list spec

  decode json = do
    { event: tag, value } <- CA.decode wireCodec json
    fromMaybe (Left $ CA.Named "event" $ CA.UnexpectedValue $ J.fromString tag) $ decodeEvents list spec tag value

-- | The envelope: `{ "event": tag, "value": json }`.
wireCodec :: JsonCodec { event :: String, value :: Json }
wireCodec = CAR.object "event" { event: CA.string, value: CA.json }
