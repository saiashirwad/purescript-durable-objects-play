module Cloudflare.Durable.Storage
  ( Key
  , Prefix
  , at
  , delete
  , deleteAll
  , get
  , key
  , keyWith
  , list
  , listWith
  , prefix
  , prefixWith
  , put
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec, codec)
import Cloudflare.Durable.Runtime (class MonadRuntime, Runtime, State(..), decodedOr, liftRuntime, platform)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Traversable (traverse)

newtype Key a = Key { name :: String, codec :: JsonCodec a }

key :: forall a. HasCodec a => String -> Key a
key = keyWith codec

keyWith :: forall a. JsonCodec a -> String -> Key a
keyWith c name = Key { name, codec: c }

-- | A family of keys sharing a prefix and a codec: `prefix "msg:" `at` "42"`.
newtype Prefix a = Prefix { prefix :: String, codec :: JsonCodec a }

prefix :: forall a. HasCodec a => String -> Prefix a
prefix = prefixWith codec

prefixWith :: forall a. JsonCodec a -> String -> Prefix a
prefixWith c p = Prefix { prefix: p, codec: c }

at :: forall a. Prefix a -> String -> Key a
at (Prefix p) suffix = Key { name: p.prefix <> suffix, codec: p.codec }

get :: forall m a. MonadRuntime m => State -> Key a -> m (Maybe a)
get (State s) (Key k) = liftRuntime do
  let operation = operationOn "get" k.name
  stored <- platform operation $ s.get k.name
  traverse (decodeAs operation k.codec) stored

put :: forall m a. MonadRuntime m => State -> Key a -> a -> m Unit
put (State s) (Key k) value = liftRuntime $ platform (operationOn "put" k.name) $ s.put k.name (CA.encode k.codec value)

delete :: forall m a. MonadRuntime m => State -> Key a -> m Boolean
delete (State s) (Key k) = liftRuntime $ platform (operationOn "delete" k.name) $ s.delete k.name

-- | Every value under the prefix, keyed by the part after it, in key order.
list :: forall m a. MonadRuntime m => State -> Prefix a -> m (Map String a)
list = listWith { limit: Nothing, reverse: false }

listWith :: forall m a. MonadRuntime m => { limit :: Maybe Int, reverse :: Boolean } -> State -> Prefix a -> m (Map String a)
listWith options (State s) (Prefix p) = liftRuntime do
  let operation = operationOn "list" p.prefix
  entries <- platform operation $ s.list { prefix: p.prefix, limit: options.limit, reverse: options.reverse }
  Map.fromFoldable <$> traverse (traverse (decodeAs operation p.codec) <<< lmap (dropPrefix p.prefix)) entries

deleteAll :: forall m. MonadRuntime m => State -> m Unit
deleteAll (State s) = liftRuntime $ platform "storage.deleteAll" s.deleteAll

-- | The operation name a failure reports, `storage.get "balance"`.
operationOn :: String -> String -> String
operationOn verb name = "storage." <> verb <> " " <> show name

decodeAs :: forall a. String -> JsonCodec a -> Json -> Runtime a
decodeAs operation c = decodedOr operation <<< CA.decode c

dropPrefix :: String -> String -> String
dropPrefix p name = fromMaybe name $ stripPrefix (Pattern p) name
