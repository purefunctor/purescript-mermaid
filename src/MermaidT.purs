-- | A higher-order `Free` monad for omitting impure computations
-- | within stacks of monad transformers.
module MermaidT where

import Prelude

import Control.Monad.Cont (class MonadTrans)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
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
-- | * a : Type
-- | + The type of the result.
-- |
-- | Constructors:
-- |
-- | * LiftImpure (Unit -> a) (t Effect a)
-- | + A `lift` instruction that either runs an `Effect` or falls back
-- |   to a default value if the `MermaidT` is interpreted into `ST`.
-- |
-- | * LiftPure (t (ST Global) a)
-- | + A `lift` instruction that runs an `ST Global`.
data MermaidF :: ((Type -> Type) -> Type -> Type) -> Type -> Type
data MermaidF t a
  = LiftImpure (Unit -> a) (t Effect a)
  | LiftPure (t (ST Global) a)

derive instance (Functor (t Effect), Functor (t (ST Global))) => Functor (MermaidF t)

-- | A free monad for omitting impure computations.
-- |
-- | Parameters:
-- |
-- | * t : (Type -> Type) -> Type -> Type
-- | + A monad transformer.
-- |
-- | * a : Type
-- | + The type of the result.
newtype MermaidT t a = MermaidT (Free (MermaidF t) a)

derive newtype instance Functor (MermaidT t)
derive newtype instance Apply (MermaidT t)
derive newtype instance Applicative (MermaidT t)
derive newtype instance Bind (MermaidT t)
derive newtype instance Monad (MermaidT t)

-- | Lift a `t Effect a` or fall back to a value.
liftImpureOrT :: forall t a. (Unit -> a) -> t Effect a -> MermaidT t a
liftImpureOrT fallback effect = MermaidT $ liftF $ LiftImpure fallback effect

-- | Lift a `t Effect a` or fall back to `mempty`.
liftImpureT :: forall t a. Monoid a => t Effect a -> MermaidT t a
liftImpureT = MermaidT <<< liftF <<< LiftImpure (const mempty)

-- | Lift a `t Effect a` or fall back to `Nothing`.
liftImpureMaybeT :: forall t a. Functor (t (Effect)) => t Effect a -> MermaidT t (Maybe a)
liftImpureMaybeT = MermaidT <<< liftF <<< LiftImpure (const Nothing) <<< map Just

-- | Lift a `t (ST Global) a`.
liftPureT :: forall t a. t (ST Global) a -> MermaidT t a
liftPureT = MermaidT <<< liftF <<< LiftPure

-- | Interpret `MermaidT` into a `t Effect`.
runImpureT
  :: forall t a
   . MonadTrans t
  => MonadRec (t Effect)
  => MonadRec (t (ST Global))
  => (forall m. t m ~> m)
  -> MermaidT t a
  -> t Effect a
runImpureT runT (MermaidT action) = runFreeM impureN action
  where
  impureN :: MermaidF _ _ -> t Effect _
  impureN = case _ of
    LiftImpure _ a -> a
    LiftPure a -> lift $ toEffect $ runT a

-- | Interpret `MermaidT` into `Effect`.
runImpureT'
  :: forall t a
   . MonadTrans t
  => MonadRec (t Effect)
  => MonadRec (t (ST Global))
  => (forall m. t m ~> m)
  -> MermaidT t a
  -> Effect a
runImpureT' runT = runT <<< runImpureT runT

-- | Interpret `MermaidT` into `t (ST Global)`.
runPureT
  :: forall t a
   . MonadRec (t Effect)
  => MonadRec (t (ST Global))
  => MermaidT t a
  -> t (ST Global) a
runPureT (MermaidT action) = runFreeM pureN action
  where
  pureN :: MermaidF _ _ -> t (ST Global) _
  pureN = case _ of
    LiftImpure f _ -> pure $ f unit
    LiftPure a -> a

-- | Interpret `MermaidT` into `ST Global`.
runPureT'
  :: forall t a
   . MonadRec (t Effect)
  => MonadRec (t (ST Global))
  => (forall m. t m ~> m)
  -> MermaidT t a
  -> ST Global a
runPureT' = (_ <<< runPureT)
