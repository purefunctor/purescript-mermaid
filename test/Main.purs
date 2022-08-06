module Test.Main where

import Prelude

import Control.Monad.Identity.Trans (runIdentityT)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Internal as STRef
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import MermaidT (MermaidT, liftImpureT, liftPureT, runImpureT', runPureT')
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

pureImpure :: forall t. MonadTrans t => Ref Boolean -> STRef Global Boolean -> MermaidT t Unit
pureImpure efRef stRef = do
  liftImpureT $ lift do
    Ref.write true efRef
  liftPureT $ lift do
    void $ STRef.write true stRef

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "MermaidT" do
    describe "runPureT" do
      it "does not run Effect" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        toEffect $ runPureT' runIdentityT $ pureImpure efRef stRef

        efVal <- Ref.read efRef
        stVal <- toEffect $ STRef.read stRef

        stVal `shouldEqual` true
        efVal `shouldEqual` false
    describe "runImpureT" do
      it "runs ST and Effect" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        runImpureT' runIdentityT $ pureImpure efRef stRef

        stVal <- toEffect $ STRef.read stRef
        efVal <- Ref.read efRef

        stVal `shouldEqual` true
        efVal `shouldEqual` true
