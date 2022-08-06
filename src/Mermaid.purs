-- | A free monad for omitting impure computations.
module Mermaid where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.ST (Region, ST)
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | The core of the `Mermaid` free monad.
-- |
-- | Parameters:
-- |
-- | * r : Region
-- | + The `ST` region.
-- |
-- | * a : Type
-- | + The type of the result.
-- |
-- | Constructors:
-- |
-- | * LiftImpure (Unit -> a) (Effect a)
-- | + A `lift` instruction that either runs an `Effect` or falls back
-- |   to a default value if the `Mermaid` is interpreted into `ST`.
-- |
-- | * LiftPure (ST r a)
-- | + A `lift` instruction that runs an `ST r`.
data MermaidF :: Region -> Type -> Type
data MermaidF r a
  = LiftImpure (Unit -> a) (Effect a)
  | LiftPure (ST r a)

derive instance Functor (MermaidF r)

-- | A free monad for omitting impure computations.
-- |
-- | Parameters:
-- |
-- | * r : Region
-- | + The `ST` region.
-- |
-- | * a : Type
-- | + The type of the result.
newtype Mermaid r a = Mermaid (Free (MermaidF r) a)

derive newtype instance Functor (Mermaid r)
derive newtype instance Apply (Mermaid r)
derive newtype instance Applicative (Mermaid r)
derive newtype instance Bind (Mermaid r)
derive newtype instance Monad (Mermaid r)

instance MonadST r (Mermaid r) where
  liftST = liftPure

instance (MonadAsk e (ST r)) => MonadAsk e (Mermaid r) where
  ask = liftPure ask

-- | Lift a `Effect a` or fall back to a value.
liftImpureOr :: forall r a. (Unit -> a) -> Effect a -> Mermaid r a
liftImpureOr fallback effect = Mermaid $ liftF $ LiftImpure fallback effect

-- | Lift a `Effect a` or fall back to `mempty`.
liftImpure :: forall r a. Monoid a => Effect a -> Mermaid r a
liftImpure = Mermaid <<< liftF <<< LiftImpure (const mempty)

-- | Lift a `Effect a` or fall back to `Nothing`.
liftImpureMaybe :: forall t r a. Functor (t (Effect)) => Effect a -> Mermaid r (Maybe a)
liftImpureMaybe = Mermaid <<< liftF <<< LiftImpure (const Nothing) <<< map Just

-- | Lift a `ST r a`.
liftPure :: forall r a. ST r a -> Mermaid r a
liftPure = Mermaid <<< liftF <<< LiftPure

-- | Interpret `Mermaid` into a `Effect`.
runImpure
  :: forall a
   . Mermaid Global a
  -> Effect a
runImpure (Mermaid action) = runFreeM impureN action
  where
  impureN :: MermaidF _ _ -> Effect _
  impureN = case _ of
    LiftImpure _ a -> a
    LiftPure a -> toEffect a

-- | Interpret `Mermaid` into `ST r`.
runPure
  :: forall r a
   . Mermaid r a
  -> ST r a
runPure (Mermaid action) = runFreeM pureN action
  where
  pureN :: MermaidF _ _ -> ST r _
  pureN = case _ of
    LiftImpure f _ -> pure $ f unit
    LiftPure a -> a
