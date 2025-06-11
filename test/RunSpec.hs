module RunSpec (spec) where

import           Helper

import           Control.Concurrent.Async
import           Session (Config(..))

import           EventQueue

import           Run hiding (defaultRunArgs)
import qualified Run

defaultRunArgs :: IO RunArgs
defaultRunArgs = do
  args <- Run.defaultRunArgs Nothing mempty
  return args { sessionConfig = args.sessionConfig { configEcho = silent } }

unwrapExceptionInLinkedThread :: IO a -> IO a
unwrapExceptionInLinkedThread = try >=> \ case
  Left (ExceptionInLinkedThread _ e) -> throwIO e
  Right a -> return a

spec :: Spec
spec = around_ unwrapExceptionInLinkedThread do
  describe "emitTriggerAndWaitForDelivery" do
    it "blocks until the test run has started" $ do
      runArgs <- defaultRunArgs

      let
        testRunInProgress :: IO Bool
        testRunInProgress = isEmptyMVar runArgs.lastOutput

        userInput :: IO ()
        userInput = do
          emitTriggerAndWaitForDelivery runArgs.queue
          testRunInProgress `shouldReturn` True
          emitEvent runArgs.queue Done

      withAsync userInput \ user -> do
        link user
        timeout $ runWith runArgs
        wait user

  describe "runWith" $ do
    it "populates modules" $ do
      runArgs <- defaultRunArgs
      emitEvent runArgs.queue Done
      timeout $ runWith runArgs
      modules <- readIORef runArgs.modules
      "Prelude" `elem` modules `shouldBe` True

    context "on Done" $ do
      it "terminates" $ do
        runArgs <- defaultRunArgs
        emitEvent runArgs.queue Done
        timeout $ runWith runArgs

    context "on RestartWith" $ do
      it "restarts the session with the given extra arguments" $ do
        withSpy $ \ spy -> do
          RunArgs{..} <- defaultRunArgs
          let
            runArgs = RunArgs {
              withSession = \ conf sessionArgs action -> withSession conf sessionArgs $ \ session -> do
                spy sessionArgs
                action session <* emitEvent runArgs.queue Done
            , ..
            }
          emitEvent runArgs.queue (RestartWith ["-Wall"])
          timeout $ runWith runArgs
        `shouldReturn` [[], ["-Wall"]]

      context "with multiple occurrences of RestartWith" $ do
        it "gives the last occurrence precedence" $ do
          withSpy $ \ spy -> do
            RunArgs{..} <- defaultRunArgs
            let
              runArgs = RunArgs {
                withSession = \ conf sessionArgs action -> withSession conf sessionArgs $ \ session -> do
                  spy sessionArgs
                  action session <* emitEvent runArgs.queue Done
              , ..
              }
            emitEvent runArgs.queue (RestartWith ["-Wall"])
            emitEvent runArgs.queue (RestartWith ["-Wdefault"])
            timeout $ runWith runArgs
          `shouldReturn` [[], ["-Wdefault"]]

      context "after a restart without arguments" $ do
        it "remembers the last seen extra arguments" $ do
          nextEvent <- stubAction [RestartWith ["-Wall"], FileEvent FileModified ".ghci", Done]
          withSpy $ \ spy -> do
            RunArgs{..} <- defaultRunArgs
            let
              runArgs = RunArgs {
                withSession = \ conf sessionArgs action -> withSession conf sessionArgs $ \ session -> do
                  spy sessionArgs
                  nextEvent >>= emitEvent runArgs.queue
                  action session
              , ..
              }
            timeout $ runWith runArgs
          `shouldReturn` [[], ["-Wall"], ["-Wall"]]
