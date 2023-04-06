module RunSpec (spec) where

import           Helper

import           EventQueue

import           Language.Haskell.GhciWrapper (Config(..))
import           Run hiding (defaultRunArgs)
import qualified Run

defaultRunArgs :: IO RunArgs
defaultRunArgs = do
  args <- Run.defaultRunArgs "startup.ghci"
  return args { ignoreConfig = True, sessionConfig = args.sessionConfig { configEcho = silent } }

spec :: Spec
spec = do
  describe "runWith" $ do
    context "on Done" $ do
      it "terminates" $ do
        runArgs <- defaultRunArgs
        emitEvent runArgs.queue Done
        timeout (runWith runArgs) `shouldReturn` Just ()

    context "on RestartWith" $ do
      it "restarts the session with the given extra arguments" $ do
        withSpy $ \ spy -> do
          RunArgs{..} <- defaultRunArgs
          let
            runArgs = RunArgs {
              withSession = \ config sessionArgs action -> withSession config sessionArgs $ \ session -> do
                spy sessionArgs
                action session <* emitEvent runArgs.queue Done
            , ..
            }
          emitEvent runArgs.queue (RestartWith ["-Wall"])
          timeout (runWith runArgs) `shouldReturn` Just ()
        `shouldReturn` [[], ["-Wall"]]

      context "with multiple occurrences of RestartWith" $ do
        it "gives the last occurrence precedence" $ do
          withSpy $ \ spy -> do
            RunArgs{..} <- defaultRunArgs
            let
              runArgs = RunArgs {
                withSession = \ config sessionArgs action -> withSession config sessionArgs $ \ session -> do
                  spy sessionArgs
                  action session <* emitEvent runArgs.queue Done
              , ..
              }
            emitEvent runArgs.queue (RestartWith ["-Wall"])
            emitEvent runArgs.queue (RestartWith ["-Wdefault"])
            timeout (runWith runArgs) `shouldReturn` Just ()
          `shouldReturn` [[], ["-Wdefault"]]

      context "after a restart without arguments" $ do
        it "remembers the last seen extra arguments" $ do
          nextEvent <- stubAction [RestartWith ["-Wall"], FileEvent FileModified ".ghci", Done]
          withSpy $ \ spy -> do
            RunArgs{..} <- defaultRunArgs
            let
              runArgs = RunArgs {
                withSession = \ config sessionArgs action -> withSession config sessionArgs $ \ session -> do
                  spy sessionArgs
                  nextEvent >>= emitEvent runArgs.queue
                  action session
              , ..
              }
            timeout (runWith runArgs) `shouldReturn` Just ()
          `shouldReturn` [[], ["-Wall"], ["-Wall"]]
