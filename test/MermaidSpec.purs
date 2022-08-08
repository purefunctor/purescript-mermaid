module Test.MermaidSpec where

import Prelude

import Control.Monad.ST.Global (toEffect)
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Ref as STRef
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mermaid (Mermaid, liftImpure, liftPure, runImpure, runPure)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

pureImpure :: forall r. Ref Boolean -> STRef r Boolean -> Mermaid r Unit
pureImpure efRef stRef = do
  liftImpure $ do
    Ref.write true efRef
  liftPure $ do
    void $ STRef.write true stRef

mermaidSpec :: Spec Unit
mermaidSpec = do
  describe "Mermaid" do
    describe "runPure" do
      it "should perform only ST" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        toEffect $ runPure $ pureImpure efRef stRef

        efVal <- Ref.read efRef
        stVal <- toEffect $ STRef.read stRef

        stVal `shouldEqual` true
        efVal `shouldEqual` false
    describe "runImpure" do
      it "should perform both Effect and ST" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        runImpure $ pureImpure efRef stRef

        stVal <- toEffect $ STRef.read stRef
        efVal <- Ref.read efRef

        stVal `shouldEqual` true
        efVal `shouldEqual` true
