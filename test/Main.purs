module Test.Main where

import Prelude

import Control.Monad.Identity.Trans (runIdentityT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (toEffect)
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Internal as STRef
import Control.Monad.State.Trans (class MonadState, evalStateT, get, modify_)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mermaid (Mermaid, liftImpure, liftPure, runImpure, runPure)
import MermaidT (MermaidT, liftImpureT, liftPureT, runImpureT, runPureT)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

pureImpure :: forall r. Ref Boolean -> STRef r Boolean -> Mermaid r Unit
pureImpure efRef stRef = do
  liftImpure $ do
    Ref.write true efRef
  liftPure $ do
    void $ STRef.write true stRef

pureImpureT :: forall t r. MonadTrans t => Ref Boolean -> STRef r Boolean -> MermaidT t r Unit
pureImpureT efRef stRef = do
  liftImpureT $ lift do
    Ref.write true efRef
  liftPureT $ lift do
    void $ STRef.write true stRef

withTransmuteT
  :: forall t r
   . MonadState Int (t Effect)
  => MonadState Int (t (ST r))
  => MermaidT t r { a :: Int, b :: Int }
withTransmuteT = do
  liftImpureT do
    modify_ (_ + 10)
  a <- liftPureT do
    get
  liftImpureT do
    modify_ (_ + 10)
  liftPureT do
    modify_ (_ + 10)
  b <- liftPureT do
    get
  pure { a, b }

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Mermaid" do
    describe "runPure" do
      it "does not run Effect" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        toEffect $ runPure $ pureImpure efRef stRef

        efVal <- Ref.read efRef
        stVal <- toEffect $ STRef.read stRef

        stVal `shouldEqual` true
        efVal `shouldEqual` false
    describe "runImpure" do
      it "runs ST and Effect" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        runImpure $ pureImpure efRef stRef

        stVal <- toEffect $ STRef.read stRef
        efVal <- Ref.read efRef

        stVal `shouldEqual` true
        efVal `shouldEqual` true
  describe "MermaidT" do
    describe "runPureT" do
      it "does not run Effect" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        toEffect $ runIdentityT $ runPureT $ pureImpureT efRef stRef

        efVal <- Ref.read efRef
        stVal <- toEffect $ STRef.read stRef

        stVal `shouldEqual` true
        efVal `shouldEqual` false
      it "propagates state" $ liftEffect do
        { a, b } <- toEffect $ flip evalStateT 0 $ runPureT withTransmuteT
        a `shouldEqual` 0
        b `shouldEqual` 10
    describe "runImpureT" do
      it "runs ST and Effect" $ liftEffect do
        efRef <- Ref.new false
        stRef <- toEffect $ STRef.new false

        -- TODO: MFunctor IdentityT
        flip runReaderT unit $ runImpureT $ pureImpureT efRef stRef

        stVal <- toEffect $ STRef.read stRef
        efVal <- Ref.read efRef

        stVal `shouldEqual` true
        efVal `shouldEqual` true
      it "propagates state" $ liftEffect do
        { a, b } <- flip evalStateT 0 $ runImpureT withTransmuteT
        a `shouldEqual` 10
        b `shouldEqual` 30
