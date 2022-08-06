module Mermaid where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Maybe (Maybe(..))
import Effect (Effect)

data MermaidF a
  = LiftImpure (Unit -> a) (Effect a)
  | LiftPure (ST Global a)

derive instance Functor MermaidF

type Mermaid = Free MermaidF

liftImpure :: forall a. Monoid a => Effect a -> Mermaid a
liftImpure = liftF <<< LiftImpure (const mempty)

liftImpureMaybe :: forall a. Effect a -> Mermaid (Maybe a)
liftImpureMaybe = liftF <<< LiftImpure (const Nothing) <<< map Just

liftPure :: forall a. ST Global a -> Mermaid a
liftPure = liftF <<< LiftPure

runImpure :: forall a. Mermaid a -> Effect a
runImpure = runFreeM impureN
  where
  impureN :: MermaidF _ -> Effect _
  impureN = case _ of
    LiftImpure _ a -> a
    LiftPure a -> toEffect a

runPure :: forall a. Mermaid a -> ST Global a
runPure = runFreeM pureN
  where
  pureN :: MermaidF _ -> ST Global _
  pureN = case _ of
    LiftImpure f _ -> pure $ f unit
    LiftPure a -> a
