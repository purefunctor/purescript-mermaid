module Mermaid
  ( Mermaid
  , liftImpure
  , liftImpureMaybe
  , liftImpureOr
  , liftPure
  , runImpure
  , runPure
  )
  where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (Region, ST)
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | A monad for omitting impure computations.
newtype Mermaid :: Region -> Type -> Type
newtype Mermaid r a = Mermaid
  ( forall k. Fn3 (Fn2 (Unit -> k) (Effect (Unit -> k)) k) (ST r (Unit -> k) -> k) (a -> k) k
  )

instance Functor (Mermaid r) where
  map f (Mermaid m) = Mermaid
    ( mkFn3 \lfEf lfSt done -> runFn3 m lfEf lfSt \a -> done (f a)
    )

instance Apply (Mermaid r) where
  apply (Mermaid mf) (Mermaid ma) = Mermaid
    ( mkFn3 \lfEf lfSt done ->
        runFn3 mf lfEf lfSt \f ->
          runFn3 ma lfEf lfSt \a ->
            done (f a)
    )

instance Applicative (Mermaid r) where
  pure a = Mermaid
    ( mkFn3 \_ _ done -> done a
    )

instance Bind (Mermaid r) where
  bind (Mermaid m) f = Mermaid
    ( mkFn3 \lfEf lfSt done ->
        runFn3 m lfEf lfSt \a ->
          case f a of
            Mermaid n ->
              runFn3 n lfEf lfSt \b ->
                done b
    )

instance Monad (Mermaid r)

instance MonadST r (Mermaid r) where
  liftST = liftPure

data RunMermaid r a
  = LiftEffect (Unit -> RunMermaid r a) (Effect (Unit -> RunMermaid r a))
  | LiftST (ST r (Unit -> RunMermaid r a))
  | Stop a

-- | Lift an `Effect` to `Mermaid`, returning `mempty` when omitted.
liftImpure :: forall r a. Monoid a => Effect a -> Mermaid r a
liftImpure m = Mermaid
  ( mkFn3 \lfEf _ done ->
      runFn2 lfEf (\_ -> done mempty) (map (\a _ -> done a) m)
  )

-- | Lift an `Effect` to `Mermaid`, returning an `a` when omitted.
liftImpureOr :: forall r a. (Unit -> a) -> Effect a -> Mermaid r a
liftImpureOr f m = Mermaid
  ( mkFn3 \lfEf _ done ->
      runFn2 lfEf (\_ -> done (f unit)) (map (\a _ -> done a) m)
  )

-- | Lift an `Effect` to `Mermaid`, returning `Nothing` when omitted.
liftImpureMaybe :: forall r a. Effect a -> Mermaid r (Maybe a)
liftImpureMaybe m = Mermaid
  ( mkFn3 \lfEf _ done ->
      runFn2 lfEf (\_ -> done Nothing) (map (\a _ -> done (Just a)) m)
  )

-- | Lift an `ST r` to `Mermaid`.
liftPure :: forall r a. ST r a -> Mermaid r a
liftPure m = Mermaid
  ( mkFn3 \_ lfSt done ->
      lfSt (map (\a _ -> done a) m)
  )

-- | Interpret `Mermaid` as an `Effect`.
runImpure :: forall a. Mermaid Global a -> Effect a
runImpure (Mermaid m) =
  let
    go step = case step unit of
      LiftEffect _ e -> Loop <$> e
      LiftST s -> Loop <$> toEffect s
      Stop a -> pure $ Done a
  in
    tailRecM go \_ ->
      runFn3 m (mkFn2 \f e -> LiftEffect f e) LiftST Stop

-- | Interpret `Mermaid` as an `ST`, omitting `Effect`.
runPure :: forall r a. Mermaid r a -> ST r a
runPure (Mermaid m) =
  let
    go step = case step unit of
      LiftEffect f _ -> go f
      LiftST s -> Loop <$> s
      Stop a -> pure $ Done a
  in
    tailRecM go \_ ->
      runFn3 m (mkFn2 \f e -> LiftEffect f e) LiftST Stop
