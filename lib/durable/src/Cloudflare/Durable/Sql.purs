-- | A `Statement i o` is SQL text with an encoder for its parameters and a
-- | decoder for its rows: contravariant in `i`, covariant in `o`, so it is a
-- | `Profunctor`. `Params` is `Op (Array Json)`, so parameter encoders combine
-- | with `divided` and `chosen`; `Decoder` is an applicative decoder over one row,
-- | so row decoders combine with `<$>` and `<*>`.
module Cloudflare.Durable.Sql
  ( Params
  , Decoder
  , Statement
  , column
  , columnOf
  , execute
  , first
  , noParams
  , param
  , paramOf
  , query
  , whole
  , wholeWith
  , statement
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec, codec)
import Cloudflare.Durable.Runtime (class MonadRuntime, State(..), decodedOr, liftRuntime, platform)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array (head)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Divisible (conquer)
import Data.Either (Either, note)
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe)
import Data.Op (Op(..))
import Data.Profunctor (class Profunctor)
import Data.Traversable (traverse)
import Foreign.Object as Object

type Params i = Op (Array Json) i

param :: forall a. JsonCodec a -> Params a
param c = Op \value -> [ CA.encode c value ]

paramOf :: forall a. HasCodec a => Params a
paramOf = param codec

noParams :: Params Unit
noParams = conquer

newtype Decoder a = Decoder (ReaderT Json (Either JsonDecodeError) a)

derive newtype instance functorDecoder :: Functor Decoder
derive newtype instance applyDecoder :: Apply Decoder
derive newtype instance applicativeDecoder :: Applicative Decoder

column :: forall a. String -> JsonCodec a -> Decoder a
column name c = Decoder $ ReaderT \json -> do
  fields <- note (TypeMismatch "Object") $ J.toObject json
  value <- note (AtKey name MissingValue) $ Object.lookup name fields
  CA.decode c value

columnOf :: forall a. HasCodec a => String -> Decoder a
columnOf name = column name codec

wholeWith :: forall a. JsonCodec a -> Decoder a
wholeWith c = Decoder $ ReaderT $ CA.decode c

whole :: forall a. HasCodec a => Decoder a
whole = wholeWith codec

newtype Statement i o = Statement
  { sql :: String
  , params :: Params i
  , row :: Decoder o
  }

instance profunctorStatement :: Profunctor Statement where
  dimap f g (Statement s) = Statement s { params = cmap f s.params, row = g <$> s.row }

statement :: forall i o. String -> Params i -> Decoder o -> Statement i o
statement sql params row = Statement { sql, params, row }

query :: forall m i o. MonadRuntime m => State -> Statement i o -> i -> m (Array o)
query (State s) (Statement { sql, params: Op encode, row: Decoder decode }) input = liftRuntime do
  let operation = "sql " <> show sql
  rows <- platform operation $ s.sql sql (encode input)
  decodedOr operation $ traverse (runReaderT decode) rows

first :: forall m i o. MonadRuntime m => State -> Statement i o -> i -> m (Maybe o)
first state stmt = map head <<< query state stmt

execute :: forall m i o. MonadRuntime m => State -> Statement i o -> i -> m Unit
execute state stmt = void <<< query state stmt
