module EventQueueSpec (spec) where

import           Helper

import           EventQueue

spec :: Spec
spec = do
  describe "processEvents" $ do
    around_ inTempDirectory $ do
      context "with FileEvent" $ do
        it "returns TriggerAction" $ do
          processEvents [FileEvent "foo"] `shouldReturn` TriggerAction

      context "with TriggerAll" $ do
        it "returns TriggerAllAction" $ do
          processEvents [TriggerAll] `shouldReturn` TriggerAllAction

        it "takes precedence over FileEvent" $ do
          processEvents [FileEvent "foo", TriggerAll, FileEvent "foo"] `shouldReturn` TriggerAllAction

      context "with Done" $ do
        it "returns DoneAction" $ do
          processEvents [Done] `shouldReturn` DoneAction

        it "takes precedence over TriggerAll" $ do
          processEvents [TriggerAll, Done, TriggerAll] `shouldReturn` DoneAction
