-- | A higher-order `Free` monad for omitting impure computations
-- | within stacks of monad transformers.
module MermaidT where

import Prelude

import Control.Monad.Cont (class MonadTrans)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.ST (Region, ST)
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | The core of the `MermaidT` free monad.
-- |
-- | Parameters:
-- |
-- | * t : (Type -> Type) -> Type -> Type
-- | + A monad transformer.
-- |
-- | * r : Region
-- | + The `ST` region.
-- |
-- | * a : Type
-- | + The type of the result.
-- |
-- | Constructors:
-- |
-- | * LiftImpure (Unit -> a) (t Effect a)
-- | + A `lift` instruction that either runs an `Effect` or falls back
-- |   to a default value if the `MermaidT` is interpreted into `ST`.
-- |
-- | * LiftPure (t (ST r) a)
-- | + A `lift` instruction that runs an `ST r`.
data MermaidF :: ((Type -> Type) -> Type -> Type) -> Region -> Type -> Type
data MermaidF t r a
  = LiftImpure (Unit -> a) (t Effect a)
  | LiftPure (t (ST r) a)

derive instance (Functor (t Effect), Functor (t (ST r))) => Functor (MermaidF t r)

-- | A free monad for omitting impure computations.
-- |
-- | Parameters:
-- |
-- | * t : (Type -> Type) -> Type -> Type
-- | + A monad transformer.
-- |
-- | * r : Region
-- | + The `ST` region.
-- |
-- | * a : Type
-- | + The type of the result.
newtype MermaidT t r a = MermaidT (Free (MermaidF t r) a)

derive newtype instance Functor (MermaidT t r)
derive newtype instance Apply (MermaidT t r)
derive newtype instance Applicative (MermaidT t r)
derive newtype instance Bind (MermaidT t r)
derive newtype instance Monad (MermaidT t r)

instance MonadTrans t => MonadST r (MermaidT t r) where
  liftST = liftPureT <<< lift

-- | Lift a `t Effect a` or fall back to a value.
liftImpureOrT :: forall t r a. (Unit -> a) -> t Effect a -> MermaidT t r a
liftImpureOrT fallback effect = MermaidT $ liftF $ LiftImpure fallback effect

-- | Lift a `t Effect a` or fall back to `mempty`.
liftImpureT :: forall t r a. Monoid a => t Effect a -> MermaidT t r a
liftImpureT = MermaidT <<< liftF <<< LiftImpure (const mempty)

-- | Lift a `t Effect a` or fall back to `Nothing`.
liftImpureMaybeT :: forall t r a. Functor (t (Effect)) => t Effect a -> MermaidT t r (Maybe a)
liftImpureMaybeT = MermaidT <<< liftF <<< LiftImpure (const Nothing) <<< map Just

-- | Lift a `t (ST r) a`.
liftPureT :: forall t r a. t (ST r) a -> MermaidT t r a
liftPureT = MermaidT <<< liftF <<< LiftPure

-- | Interpret `MermaidT` into a `t Effect`.
runImpureT
  :: forall t r a
   . MonadTrans t
  => MonadRec (t Effect)
  => MonadRec (t (ST r))
  => (t (ST r) ~> t Effect)
  -> MermaidT t r a
  -> t Effect a
runImpureT transmute (MermaidT action) = runFreeM impureN action
  where
  impureN :: MermaidF _ _ _ -> t Effect _
  impureN = case _ of
    LiftImpure _ a -> a
    LiftPure a -> transmute a

-- | Interpret `MermaidT` into `t (ST r)`.
runPureT
  :: forall t r a
   . MonadRec (t Effect)
  => MonadRec (t (ST r))
  => MermaidT t r a
  -> t (ST r) a
runPureT (MermaidT action) = runFreeM pureN action
  where
  pureN :: MermaidF _ _ _ -> t (ST r) _
  pureN = case _ of
    LiftImpure f _ -> pure $ f unit
    LiftPure a -> a
