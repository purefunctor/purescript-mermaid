module Test.Main where

import Prelude

import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Internal as STRef
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mermaid (Mermaid, liftImpure, liftPure, runImpure, runPure)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

pureImpure :: STRef Global Int -> Ref Int -> Mermaid Unit
pureImpure stRef efRef = do
  _ <- liftPure do
    void $ STRef.write 10000 stRef
  _ <- liftImpure do
    Ref.write 10000 efRef
  pure unit

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Mermaid" do
    describe "runPure" do
      it "does not run Effect" $ liftEffect do
        stRef <- toEffect $ STRef.new 0
        efRef <- Ref.new 0

        runPure $ pureImpure stRef efRef

        stVal <- toEffect $ STRef.read stRef
        efVal <- Ref.read efRef

        stVal `shouldEqual` 10000
        efVal `shouldEqual` 0
    describe "runImpure" do
      it "runs ST and Effect" $ liftEffect do
        stRef <- toEffect $ STRef.new 0
        efRef <- Ref.new 0

        runImpure $ pureImpure stRef efRef

        stVal <- toEffect $ STRef.read stRef
        efVal <- Ref.read efRef

        stVal `shouldEqual` 10000
        efVal `shouldEqual` 10000
