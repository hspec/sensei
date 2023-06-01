module RunSpec (spec) where

import           Helper

import           EventQueue

import           Language.Haskell.GhciWrapper (Config(..))
import           Run hiding (defaultRunArgs)
import qualified Run

defaultRunArgs :: IO RunArgs
defaultRunArgs = do
  args <- Run.defaultRunArgs "startup.ghci"
  return args { sessionConfig = args.sessionConfig { configEcho = silent } }

spec :: Spec
spec = do
  describe "runWith" $ do
    context "on Done" $ do
      it "terminates" $ do
        runArgs <- defaultRunArgs
        emitEvent runArgs.queue Done
        timeout 10_000_000 (runWith runArgs) `shouldReturn` Just ()
