module PagerSpec (spec) where

import           Helper

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
