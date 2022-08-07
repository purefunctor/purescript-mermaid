module Test.MermaidHyruleSpec where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array (length, replicate)
import Data.Array as Array
import Data.Foldable (oneOf)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event (AnEvent, Event, bang, makeEvent, subscribe)
import Mermaid (liftImpure, liftPure, runImpure, runPure)
import Mermaid.Hyrule (MdEvent, fromEfEvent, fromStEvent, toEfEvent, toStEvent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nTimes :: Int -> Ref (Array Int) -> STRef Global (Array Int) -> Event Unit
nTimes n v w =
  makeEvent \k -> do
    rRef <- toEffect $ STRef.new 0
    subscribe (oneOf $ replicate (n * 2) $ bang unit) \_ -> do
      k unit
      r <- toEffect $ STRef.read rRef
      if r < n then do
        Ref.modify_ (Array.cons r) v
      else do
        void $ toEffect $ STRef.modify (Array.cons r) w
      void $ toEffect $ STRef.modify (_ + 1) rRef

nTimes' :: Int -> Ref (Array Int) -> STRef Global (Array Int) -> AnEvent (ST Global) Unit
nTimes' n _ w =
  makeEvent \k -> do
    rRef <- STRef.new 0
    subscribe (oneOf $ replicate (n * 2) $ bang unit) \_ -> do
      k unit
      r <- STRef.read rRef
      when (r >= n) do
        void $ STRef.modify (Array.cons r) w
      void $ STRef.modify (_ + 1) rRef

nTimes'' :: forall r. Int -> Ref (Array Int) -> STRef r (Array Int) -> MdEvent r Unit
nTimes'' n v w =
  makeEvent \k -> do
    rRef <- liftPure $ STRef.new 0
    subscribe (oneOf $ replicate (n * 2) $ bang unit) \_ -> do
      k unit
      r <- liftPure $ STRef.read rRef
      if r < 5 then do
        liftImpure do
          Ref.modify_ (Array.cons r) v
      else do
        liftPure do
          void $ STRef.modify (Array.cons r) w
      liftPure do
        void $ STRef.modify (_ + 1) rRef

mermaidHyruleSpec :: Spec Unit
mermaidHyruleSpec = do
  let
    setUp :: Effect { efRef :: Ref (Array Int), stRef :: STRef Global (Array Int) }
    setUp = do
      efRef <- Ref.new []
      stRef <- toEffect $ STRef.new []
      pure { efRef, stRef }

    verify :: Ref (Array Int) -> STRef Global (Array Int) -> Int -> Int -> Effect Unit
    verify efRef stRef efLen stLen = do
      efVal <- Ref.read efRef
      length efVal `shouldEqual` efLen

      stVal <- toEffect $ STRef.read stRef
      length stVal `shouldEqual` stLen

  describe "Mermaid.Hyrule" do
    describe "fromEfEvent" do
      it "should be nullified by runPure" $ liftEffect do
        { efRef, stRef } <- setUp
        _ <- toEffect $ runPure $ subscribe (fromEfEvent $ nTimes 5 efRef stRef) pure
        verify efRef stRef 0 0
      it "should be performed by runImpure" $ liftEffect do
        { efRef, stRef } <- setUp
        _ <- runImpure $ subscribe (fromEfEvent $ nTimes 5 efRef stRef) pure
        verify efRef stRef 5 5
    describe "fromStEvent" do
      it "should be performed by runPure" $ liftEffect do
        { efRef, stRef } <- setUp
        _ <- toEffect $ runPure $ subscribe (fromStEvent $ nTimes' 5 efRef stRef) pure
        verify efRef stRef 0 5
      it "should be performed by runImpure" $ liftEffect do
        { efRef, stRef } <- setUp
        _ <- runImpure $ subscribe (fromStEvent $ nTimes' 5 efRef stRef) pure
        verify efRef stRef 0 5
    describe "toEfEvent" do
      it "should perform both Effect and ST" $ liftEffect do
        { efRef, stRef } <- setUp
        _ <- subscribe (toEfEvent $ nTimes'' 5 efRef stRef) pure
        verify efRef stRef 5 5
    describe "toStEvent" do
      it "should perform only ST" $ liftEffect do
        { efRef, stRef } <- setUp
        _ <- toEffect $ subscribe (toStEvent $ nTimes'' 5 efRef stRef) pure
        verify efRef stRef 0 5
