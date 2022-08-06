module Mermaid where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)

data MermaidF a
  = IsEffectful (Boolean -> a)
  | LiftImpure (Effect a)
  | LiftPure (ST Global a)

derive instance Functor MermaidF

type Mermaid = Free MermaidF

liftImpure :: forall a. Monoid a => Effect a -> Mermaid a
liftImpure a = do
  isEffectful <- liftF $ IsEffectful identity
  if isEffectful then
    liftF $ LiftImpure a
  else
    liftF $ LiftPure $ pure mempty

liftImpureMaybe :: forall a. Effect a -> Mermaid (Maybe a)
liftImpureMaybe a = do
  isEffectful <- liftF $ IsEffectful identity
  if isEffectful then
    liftF $ LiftImpure $ Just <$> a
  else
    liftF $ LiftPure $ pure Nothing

liftPure :: forall a. ST Global a -> Mermaid a
liftPure = liftF <<< LiftPure

runImpure :: forall a. Mermaid a -> Effect a
runImpure = runFreeM impureN
  where
  impureN :: MermaidF _ -> Effect _
  impureN = case _ of
    IsEffectful k -> pure (k true)
    LiftImpure a -> a
    LiftPure a -> toEffect a

runPure :: forall a. Mermaid a -> Effect a
runPure = runFreeM pureN
  where
  pureN :: MermaidF _ -> Effect _
  pureN = case _ of
    IsEffectful k -> pure (k false)
    LiftImpure _ -> unsafeCrashWith "Invalid!"
    LiftPure a -> toEffect a
