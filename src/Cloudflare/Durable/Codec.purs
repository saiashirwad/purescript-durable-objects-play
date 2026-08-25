module Cloudflare.Durable.Codec
  ( class HasCodec
  , class HasFieldCodecs
  , codec
  , fieldCodecs
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Type.Proxy (Proxy(..))

class HasCodec a where
  codec :: JsonCodec a

instance hasCodecInt :: HasCodec Int where
  codec = CA.int

instance hasCodecNumber :: HasCodec Number where
  codec = CA.number

instance hasCodecString :: HasCodec String where
  codec = CA.string

instance hasCodecBoolean :: HasCodec Boolean where
  codec = CA.boolean

instance hasCodecUnit :: HasCodec Unit where
  codec = CA.null

-- | Nothing to encode, nothing decodes: the error type of a method that
-- | cannot fail.
instance hasCodecVoid :: HasCodec Void where
  codec = CA.prismaticCodec "Void" (const Nothing) absurd CA.json

instance hasCodecArray :: HasCodec a => HasCodec (Array a) where
  codec = CA.array codec

-- | `Nothing` is `null` on the wire, so hand-written clients can send it.
instance hasCodecMaybe :: HasCodec a => HasCodec (Maybe a) where
  codec = Compat.maybe codec

instance hasCodecRecord :: (RowToList r list, HasFieldCodecs list r) => HasCodec (Record r) where
  codec = CA.object "Record" $ fieldCodecs (Proxy :: Proxy list)

class HasFieldCodecs (list :: RowList Type) (r :: Row Type) | list -> r where
  fieldCodecs :: Proxy list -> CA.JPropCodec (Record r)

instance hasFieldCodecsNil :: HasFieldCodecs Nil () where
  fieldCodecs _ = CA.record

instance hasFieldCodecsCons ::
  ( IsSymbol name
  , HasCodec a
  , Row.Cons name a tail r
  , Row.Lacks name tail
  , HasFieldCodecs list tail
  ) =>
  HasFieldCodecs (Cons name a list) r where
  fieldCodecs _ = CA.recordProp (Proxy :: Proxy name) codec (fieldCodecs (Proxy :: Proxy list))
