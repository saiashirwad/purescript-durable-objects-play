-- | A description of a type that yields both a JSON codec and a JSON Schema,
-- | so a tool's parameters are decoded and advertised from one definition.
-- | `Schema` is an `Invariant` functor: `imap` wraps and unwraps newtypes.
module Ai.Schema
  ( Schema
  , array
  , boolean
  , class Fields
  , codec
  , describe
  , fields
  , int
  , json
  , number
  , nullable
  , object
  , oneOf
  , properties
  , string
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array (elem)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Record as Record
import Type.Proxy (Proxy(..))

newtype Schema a = Schema { json :: Json, codec :: JsonCodec a }

instance invariantSchema :: Invariant Schema where
  imap f g (Schema s) = Schema s { codec = dimap g f s.codec }

json :: forall a. Schema a -> Json
json (Schema s) = s.json

codec :: forall a. Schema a -> JsonCodec a
codec (Schema s) = s.codec

-- | Add a description the model will read.
describe :: forall a. String -> Schema a -> Schema a
describe text (Schema s) = Schema s { json = with "description" (J.fromString text) s.json }

with :: String -> Json -> Json -> Json
with key value = J.caseJsonObject (J.fromObject (Object.singleton key value)) (J.fromObject <<< Object.insert key value)

primitive :: forall a. String -> JsonCodec a -> Schema a
primitive kind c = Schema { json: J.fromObject (Object.singleton "type" (J.fromString kind)), codec: c }

string :: Schema String
string = primitive "string" CA.string

int :: Schema Int
int = primitive "integer" CA.int

number :: Schema Number
number = primitive "number" CA.number

boolean :: Schema Boolean
boolean = primitive "boolean" CA.boolean

array :: forall a. Schema a -> Schema (Array a)
array (Schema s) = Schema
  { json: J.fromObject $ Object.fromFoldable [ "type" /\ J.fromString "array", "items" /\ s.json ]
  , codec: CA.array s.codec
  }

nullable :: forall a. Schema a -> Schema (Maybe a)
nullable (Schema s) = Schema
  { json: J.fromObject $ Object.singleton "anyOf" $ J.fromArray [ s.json, J.fromObject (Object.singleton "type" (J.fromString "null")) ]
  , codec: Compat.maybe s.codec
  }

-- | One of a fixed set of strings.
oneOf :: Array String -> Schema String
oneOf choices = Schema
  { json: J.fromObject $ Object.fromFoldable [ "type" /\ J.fromString "string", "enum" /\ J.fromArray (J.fromString <$> choices) ]
  , codec: CA.prismaticCodec "oneOf" (\s -> if s `elem` choices then Just s else Nothing) identity CA.string
  }

-- | A record schema from a record of schemas: `object { name: string, age: int }`.
-- | Every field is required, as strict tool schemas want.
object :: forall spec list r. RowToList spec list => Fields list spec r => Record spec -> Schema (Record r)
object spec = Schema
  { json: J.fromObject $ Object.fromFoldable
      [ "type" /\ J.fromString "object"
      , "properties" /\ J.fromObject (Object.fromFoldable (fst' <$> props))
      , "required" /\ J.fromArray (J.fromString <<< _.name <$> props)
      , "additionalProperties" /\ J.fromBoolean false
      ]
  , codec: CA.object "Record" $ fields (Proxy :: Proxy list) spec
  }
  where
  props = properties (Proxy :: Proxy list) spec
  fst' p = p.name /\ p.schema

class Fields (list :: RowList Type) (spec :: Row Type) (r :: Row Type) | list -> r where
  fields :: Proxy list -> Record spec -> CA.JPropCodec (Record r)
  properties :: Proxy list -> Record spec -> Array { name :: String, schema :: Json }

instance fieldsNil :: Fields Nil spec () where
  fields _ _ = CA.record
  properties _ _ = []

instance fieldsCons ::
  ( IsSymbol name
  , Row.Cons name (Schema a) specTail spec
  , Row.Cons name a tail r
  , Row.Lacks name tail
  , Fields list spec tail
  ) =>
  Fields (Cons name (Schema a) list) spec r where
  fields _ spec = CA.recordProp name (codec (Record.get name spec)) (fields (Proxy :: Proxy list) spec)
    where
    name = Proxy :: Proxy name
  properties _ spec =
    [ { name: reflectSymbol name, schema: json (Record.get name spec) } ] <> properties (Proxy :: Proxy list) spec
    where
    name = Proxy :: Proxy name
