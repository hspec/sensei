{-# LANGUAGE OverloadedLists #-}
module EventQueueSpec (spec) where

import           Helper
import qualified System.Info
import           Data.List.NonEmpty (NonEmpty)

import           Run (watchFiles)

import           EventQueue hiding (processQueue, processEvents)
import qualified EventQueue

withGitRepository :: (FilePath -> IO a) -> IO a
withGitRepository action = withTempDirectory $ \ dir -> do
  readProcess "git" ["-C", dir, "init", "--initial-branch=main"] "" >> action dir

spec :: Spec
spec = do
  describe "processQueue" $ do
    let
      processQueue :: FilePath -> EventQueue -> IO Status
      processQueue dir queue = do
        (notify, status) <- EventQueue.processQueue pass silent dir queue (.notify) (.notify)
        notify.notify
        return status

    context "when a spec is added" $ do
      it "restarts" $ do
        when (System.Info.os == "darwin") pending
        withTempDirectory $ \ dir -> do
          queue <- newQueue
          watchFiles dir queue $ do
            threadDelay 100_000
            touch $ dir </> "FooSpec.hs"
            timeout $ processQueue dir queue `shouldReturn` Restart Nothing

    context "when a spec is removed" $ do
      it "restarts" $ do
        withTempDirectory $ \ dir -> do
          let file = dir </> "FooSpec.hs"
          touch file
          queue <- newQueue
          watchFiles dir queue $ do
            threadDelay 100_000
            removeFile file
            timeout $ processQueue dir queue `shouldReturn` Restart Nothing

    context "when .ghci is modified" $ do
      it "restarts" $ do
        withTempDirectory $ \ dir -> do
          let file = dir </> ".ghci"
          touch file
          queue <- newQueue
          watchFiles dir queue $ do
            threadDelay 100_000
            touch file
            timeout $ processQueue dir queue `shouldReturn` Restart Nothing

  describe "processEvents" $ do
    let
      processEvents :: FilePath -> NonEmpty Event -> IO (Maybe Action)
      processEvents dir = fmap snd . EventQueue.processEvents silent dir

    around withGitRepository $ do

      context "with FileEvent" $ do
        it "returns TriggerAction" $ \ dir -> do
          processEvents dir [FileEvent FileModified "test/FooSpec.hs"]
            `shouldReturn` Just (TriggerAction ["test/FooSpec.hs"])

        context "with git ignored files" $ do
          it "returns NoneAction" $ \ dir -> do
            writeFile (dir </> ".gitignore") "test/FooSpec.hs"
            processEvents dir [FileEvent FileModified "test/FooSpec.hs"]
              `shouldReturn` Nothing

        context "when a Spec file is added" $ do
          it "returns RestartAction" $ \ dir -> do
            processEvents dir [FileEvent FileAdded "test/FooSpec.hs"]
              `shouldReturn` Just (RestartAction "test/FooSpec.hs" FileAdded)

          it "takes precedence over TriggerAll" $ \ dir -> do
            processEvents dir [TriggerAll, FileEvent FileAdded "test/FooSpec.hs", TriggerAll]
              `shouldReturn` Just (RestartAction "test/FooSpec.hs" FileAdded)

          it "is overruled by Done" $ \ dir -> do
            processEvents dir [Done, FileEvent FileAdded "test/FooSpec.hs", Done]
              `shouldReturn` Just DoneAction

        context "when a Spec file is removed" $ do
          it "returns RestartAction" $ \ dir -> do
            processEvents dir [FileEvent FileRemoved "test/FooSpec.hs"]
              `shouldReturn` Just (RestartAction "test/FooSpec.hs" FileRemoved)

        context "when file is first removed and then added" $ do
          it "returns TriggerAction" $ \ dir -> do
            processEvents dir [FileEvent FileRemoved "test/FooSpec.hs", FileEvent FileAdded "test/FooSpec.hs"]
              `shouldReturn` Just (TriggerAction ["test/FooSpec.hs"])

        context "when file is first added and then removed" $ do
          it "returns NoneAction" $ \ dir -> do
            processEvents dir [FileEvent FileAdded "test/FooSpec.hs", FileEvent FileRemoved "test/FooSpec.hs"]
              `shouldReturn` Nothing

      context "with Trigger" $ do
        it "returns TriggerAction" $ \ dir -> do
          processEvents dir [Trigger $ OnTestRunStarted pass]
            `shouldReturn` Just (TriggerAction [])

      context "with TriggerAll" $ do
        it "returns TriggerAllAction" $ \ dir -> do
          processEvents dir [TriggerAll]
            `shouldReturn` Just TriggerAllAction

        it "takes precedence over FileEvent" $ \ dir -> do
          processEvents dir [FileEvent FileModified "foo", TriggerAll, FileEvent FileModified "foo"]
            `shouldReturn` Just TriggerAllAction

      context "with Done" $ do
        it "returns DoneAction" $ \ dir -> do
          processEvents dir [Done]
            `shouldReturn` Just DoneAction

  describe "combineFileEvents" $ do
    it "combines removed/added to modified" $ do
      combineFileEvents [fileRemoved "foo", fileModified "bar", fileAdded "foo"]
        `shouldBe` [fileModified "foo", fileModified "bar"]

    it "does not combine events with different file names" $ do
      combineFileEvents [fileRemoved "foo", fileAdded "bar"]
        `shouldBe` [fileRemoved "foo", fileAdded "bar"]

  describe "groupFileEvents" $ do
    it "groups file event types by file name" $ do
      groupFileEvents [fileRemoved "foo", fileModified "bar", fileAdded "foo"]
        `shouldBe` [("foo", [FileRemoved, FileAdded]), ("bar", [FileModified])]

  where
    fileRemoved file = (file, FileRemoved)
    fileAdded file = (file, FileAdded)
    fileModified file = (file, FileModified)
