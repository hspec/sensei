module EventQueueSpec (spec) where

import           Helper

import           EventQueue

withGitRepository :: IO a -> IO a
withGitRepository action = inTempDirectory (readProcess "git" ["init"] "" >> action)

spec :: Spec
spec = do
  describe "processEvents" $ do
    around_ withGitRepository $ do

      context "with FileEvent" $ do
        it "returns TriggerAction" $ do
          processEvents [FileEvent FileModified "test/FooSpec.hs"] `shouldReturn` TriggerAction ["test/FooSpec.hs"]

        context "with git ignored files" $ do
          it "returns NoneAction" $ do
            writeFile ".gitignore" "test/FooSpec.hs"
            processEvents [FileEvent FileModified "test/FooSpec.hs"] `shouldReturn` NoneAction

        context "when a Spec file is added" $ do
          it "returns ReloadAction" $ do
            processEvents [FileEvent FileAdded "test/FooSpec.hs"] `shouldReturn` ReloadAction "test/FooSpec.hs" FileAdded

          it "takes precedence over TriggerAll" $ do
            processEvents [TriggerAll, FileEvent FileAdded "test/FooSpec.hs", TriggerAll] `shouldReturn` ReloadAction "test/FooSpec.hs" FileAdded

          it "is overruled by Done" $ do
            processEvents [Done, FileEvent FileAdded "test/FooSpec.hs", Done] `shouldReturn` DoneAction

        context "when a Spec file is removed" $ do
          it "returns ReloadAction" $ do
            processEvents [FileEvent FileRemoved "test/FooSpec.hs"] `shouldReturn` ReloadAction "test/FooSpec.hs" FileRemoved

        context "when file is first removed and then added" $ do
          it "returns TriggerAction" $ do
            processEvents [FileEvent FileRemoved "test/FooSpec.hs", FileEvent FileAdded "test/FooSpec.hs"] `shouldReturn` TriggerAction ["test/FooSpec.hs"]

        context "when file is first added and then removed" $ do
          it "returns NoneAction" $ do
            processEvents [FileEvent FileAdded "test/FooSpec.hs", FileEvent FileRemoved "test/FooSpec.hs"] `shouldReturn` NoneAction

      context "with TriggerAll" $ do
        it "returns TriggerAllAction" $ do
          processEvents [TriggerAll] `shouldReturn` TriggerAllAction

        it "takes precedence over FileEvent" $ do
          processEvents [FileEvent FileModified "foo", TriggerAll, FileEvent FileModified "foo"] `shouldReturn` TriggerAllAction

      context "with Done" $ do
        it "returns DoneAction" $ do
          processEvents [Done] `shouldReturn` DoneAction

  describe "combineFileEvents" $ do
    it "combines removed/added to modified" $ do
      combineFileEvents [fileRemoved "foo", fileModified "bar", fileAdded "foo"] `shouldBe` [fileModified "foo", fileModified "bar"]

    it "does not combine events with different file names" $ do
      combineFileEvents [fileRemoved "foo", fileAdded "bar"] `shouldBe` [fileRemoved "foo", fileAdded "bar"]

  describe "groupFileEvents" $ do
    it "groups file event types by file name" $ do
      groupFileEvents [fileRemoved "foo", fileModified "bar", fileAdded "foo"] `shouldBe` [("foo", [FileRemoved, FileAdded]), ("bar", [FileModified])]

  where
    fileRemoved file = (file, FileRemoved)
    fileAdded file = (file, FileAdded)
    fileModified file = (file, FileModified)
