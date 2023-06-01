module RunSpec (spec) where

import           Helper

import           EventQueue

import           Run

spec :: Spec
spec = do
  describe "runWith" $ do
    context "on Done" $ do
      it "terminates" $ do
        runArgs@RunArgs{queue} <- defaultRunArgs "startup.ghci"
        emitEvent queue Done
        timeout 10_000_000 (runWith runArgs) `shouldReturn` Just ()
