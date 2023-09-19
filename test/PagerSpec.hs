module PagerSpec (spec) where

import           Helper

import           System.IO
import           System.Process hiding (createPipe)

import           Pager

spec :: Spec
spec = do
  describe "matchOptions" $ do
    it "contains --incsearch" $ do
      -- The `--incsearch` flag changes the behavior of `--pattern` in subtle
      -- ways.  A user might have `--incsearch` enabled globally.  To ensure
      -- consistent behavior across environments we always enable
      -- `--incsearch`.
      matchOptions `shouldContain` ["--incsearch"]

  describe "pagerWith" $ do
    it "pipes the provided input into a pager" $ do
      (stdoutReadEnd, stdoutWriteEnd) <- createPipe
      let
        process :: CreateProcess
        process = (proc "cat" []) { std_out = UseHandle stdoutWriteEnd }
      bracket (pagerWith process "foo") id $ \ _ -> do
        hGetContents stdoutReadEnd `shouldReturn` "foo"

    it "can be canceled" $ do
      let
        process :: CreateProcess
        process = proc "sleep" ["1d"]
      cancel <- pagerWith process "foo"
      timeout cancel `shouldReturn` Just ()
