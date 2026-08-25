module Cloudflare.Static
  ( Static
  , build
  , plan
  , static
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.Tuple (Tuple(..))

-- | `Const plan` collects what a program asks for; `ReaderT env m` answers.
-- | Applicative only: a program cannot branch on an answer, so the plan is
-- | known before anything runs.
newtype Static :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype Static plan env m a = Static (Product (Const plan) (ReaderT env m) a)

derive newtype instance functorStatic :: Functor m => Functor (Static plan env m)
derive newtype instance applyStatic :: (Semigroup plan, Apply m) => Apply (Static plan env m)
derive newtype instance applicativeStatic :: (Monoid plan, Applicative m) => Applicative (Static plan env m)

static :: forall plan env m a. plan -> (env -> m a) -> Static plan env m a
static p answer = Static $ Product $ Tuple (Const p) (ReaderT answer)

plan :: forall plan env m a. Static plan env m a -> plan
plan (Static (Product (Tuple (Const p) _))) = p

build :: forall plan env m a. Static plan env m a -> env -> m a
build (Static (Product (Tuple _ answer))) = runReaderT answer
