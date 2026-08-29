module Cloudflare.Durable.Rpc
  ( Method(..)
  , NoError
  , Rpc(..)
  , RpcFailure(..)
  , fail
  , infallible
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
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

-- | The error type of a method that cannot fail: there is no value to encode
-- | and none to decode. `infallible` lifts such a call into any `Rpc e`.
type NoError = Void

data RpcFailure e
  = DomainError e
  | PlatformError PlatformError
  | TransportError String
  | DecodeError String
  | RemoteDefect String

derive instance functorRpcFailure :: Functor RpcFailure
derive instance eqRpcFailure :: Eq e => Eq (RpcFailure e)
derive instance genericRpcFailure :: Generic (RpcFailure e) _

instance showRpcFailure :: Show e => Show (RpcFailure e) where
  show = genericShow

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

instance bifunctorRpc :: Bifunctor Rpc where
  bimap f g (Rpc action) = Rpc $ map g $ withExceptT (map f) action

fail :: forall e a. e -> Rpc e a
fail = throwError <<< DomainError

run :: forall e a. Rpc e a -> Aff (Either (RpcFailure e) a)
run (Rpc action) = runExceptT action

infallible :: forall e a. Rpc NoError a -> Rpc e a
infallible = lmap absurd

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
