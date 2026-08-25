module Cloudflare.Durable.Rpc
  ( Method(..)
  , NoError
  , Rpc(..)
  , RpcFailure(..)
  , fail
  , infallible
  , mapError
  , method
  , methodWith
  , run
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec, codec)
import Cloudflare.Durable.Runtime (class MonadRuntime, PlatformError)
import Cloudflare.Durable.Runtime as Runtime
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype NoError = NoError Void

instance showNoError :: Show NoError where
  show (NoError v) = absurd v

instance eqNoError :: Eq NoError where
  eq (NoError v) _ = absurd v

instance hasCodecNoError :: HasCodec NoError where
  codec = CA.prismaticCodec "NoError" (const Nothing) (\(NoError v) -> absurd v) CA.json

data RpcFailure e
  = DomainError e
  | PlatformError PlatformError
  | TransportError String
  | DecodeError String
  | RemoteDefect String

derive instance functorRpcFailure :: Functor RpcFailure
derive instance eqRpcFailure :: Eq e => Eq (RpcFailure e)

instance showRpcFailure :: Show e => Show (RpcFailure e) where
  show = case _ of
    DomainError e -> "DomainError " <> show e
    PlatformError e -> "PlatformError " <> show e
    TransportError message -> "TransportError " <> show message
    DecodeError message -> "DecodeError " <> show message
    RemoteDefect message -> "RemoteDefect " <> show message

newtype Rpc e a = Rpc (ExceptT (RpcFailure e) Aff a)

derive newtype instance functorRpc :: Functor (Rpc e)
derive newtype instance applyRpc :: Apply (Rpc e)
derive newtype instance applicativeRpc :: Applicative (Rpc e)
derive newtype instance bindRpc :: Bind (Rpc e)
derive newtype instance monadRpc :: Monad (Rpc e)
derive newtype instance monadRecRpc :: MonadRec (Rpc e)
derive newtype instance monadEffectRpc :: MonadEffect (Rpc e)
derive newtype instance monadAffRpc :: MonadAff (Rpc e)
derive newtype instance monadThrowRpc :: MonadThrow (RpcFailure e) (Rpc e)
derive newtype instance monadErrorRpc :: MonadError (RpcFailure e) (Rpc e)

instance monadRuntimeRpc :: MonadRuntime (Rpc e) where
  liftRuntime action = Rpc $ ExceptT $ lmap PlatformError <$> Runtime.run action

fail :: forall e a. e -> Rpc e a
fail = throwError <<< DomainError

run :: forall e a. Rpc e a -> Aff (Either (RpcFailure e) a)
run (Rpc action) = runExceptT action

mapError :: forall e e' a. (e -> e') -> Rpc e a -> Rpc e' a
mapError f (Rpc action) = Rpc $ withExceptT (map f) action

infallible :: forall e a. Rpc NoError a -> Rpc e a
infallible = mapError \(NoError v) -> absurd v

newtype Method e req res = Method
  { request :: JsonCodec req
  , success :: JsonCodec res
  , error :: JsonCodec e
  }

method
  :: forall e req res
   . HasCodec e
  => HasCodec req
  => HasCodec res
  => Method e req res
method = methodWith { request: codec, success: codec, error: codec }

methodWith
  :: forall e req res
   . { request :: JsonCodec req, success :: JsonCodec res, error :: JsonCodec e }
  -> Method e req res
methodWith = Method
