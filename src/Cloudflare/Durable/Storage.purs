module Cloudflare.Durable.Storage
  ( Key
  , delete
  , get
  , key
  , keyWith
  , put
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec, codec)
import Cloudflare.Durable.Runtime (class MonadRuntime, State(..), liftRuntime, platform, platformError)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

newtype Key a = Key { name :: String, codec :: JsonCodec a }

key :: forall a. HasCodec a => String -> Key a
key = keyWith codec

keyWith :: forall a. JsonCodec a -> String -> Key a
keyWith c name = Key { name, codec: c }

get :: forall m a. MonadRuntime m => State -> Key a -> m (Maybe a)
get (State s) (Key k) = liftRuntime do
  let operation = "storage.get " <> show k.name
  stored <- platform operation $ s.get k.name
  case stored of
    Nothing -> pure Nothing
    Just json -> case CA.decode k.codec json of
      Right value -> pure $ Just value
      Left err -> platformError operation $ CA.printJsonDecodeError err

put :: forall m a. MonadRuntime m => State -> Key a -> a -> m Unit
put (State s) (Key k) value = liftRuntime
  $ platform ("storage.put " <> show k.name)
  $ s.put k.name (CA.encode k.codec value)

delete :: forall m a. MonadRuntime m => State -> Key a -> m Boolean
delete (State s) (Key k) = liftRuntime
  $ platform ("storage.delete " <> show k.name)
  $ s.delete k.name
