module Cloudflare.Static
  ( Static
  , asks
  , build
  , plan
  , static
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.Tuple (Tuple(..))

-- | A program in two halves: `Const plan` collects what it asks for, and
-- | `ReaderT env m` answers once the environment exists. Applicative only,
-- | so no answer can change the plan: the plan is known before anything runs.
newtype Static :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype Static plan env m a = Static (Product (Const plan) (ReaderT env m) a)

derive newtype instance functorStatic :: Functor m => Functor (Static plan env m)
derive newtype instance applyStatic :: (Semigroup plan, Apply m) => Apply (Static plan env m)
derive newtype instance applicativeStatic :: (Monoid plan, Applicative m) => Applicative (Static plan env m)

-- | A monoid lifted through the applicative: plans combine, answers combine.
instance semigroupStatic :: (Semigroup plan, Apply m, Semigroup a) => Semigroup (Static plan env m a) where
  append = lift2 append

instance monoidStatic :: (Monoid plan, Applicative m, Monoid a) => Monoid (Static plan env m a) where
  mempty = pure mempty

static :: forall plan env m a. plan -> (env -> m a) -> Static plan env m a
static p answer = Static $ Product $ Tuple (Const p) (ReaderT answer)

-- | Read the environment without asking for anything.
asks :: forall plan env m a. Monoid plan => Applicative m => (env -> a) -> Static plan env m a
asks f = static mempty (pure <<< f)

plan :: forall plan env m a. Static plan env m a -> plan
plan (Static (Product (Tuple (Const p) _))) = p

build :: forall plan env m a. Static plan env m a -> env -> m a
build (Static (Product (Tuple _ answer))) = runReaderT answer
