module Mermaid
  ( module Mermaid
  , module Mermaid.Do
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
import Effect (Effect)
import Mermaid.Do (bind, discard, pure) as Mermaid.Do

data MermaidF a
  = LiftImpure (Effect a)
  | LiftPure (ST Global a)

derive instance Functor MermaidF

type Mermaid = Free MermaidF

liftImpure :: forall a. Effect a -> Mermaid a
liftImpure = liftF <<< LiftImpure

liftPure :: forall a. ST Global a -> Mermaid a
liftPure = liftF <<< LiftPure

runImpure :: Mermaid Unit -> Effect Unit
runImpure = runFreeM impureN
  where
  impureN :: MermaidF _ -> Effect _
  impureN = case _ of
    LiftImpure a -> a
    LiftPure a -> toEffect a

runPure :: Mermaid Unit -> Effect Unit
runPure = runFreeM pureN
  where
  pureN :: MermaidF _ -> Effect _
  pureN = case _ of
    LiftImpure _ -> mempty
    LiftPure a -> toEffect a
