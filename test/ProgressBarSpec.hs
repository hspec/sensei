module ProgressBarSpec (spec) where

import           Helper

import           ProgressBar

spec :: Spec
spec = do
  describe "progress" $ do
    it "" $ do
      progress 10 100 `shouldBe` "▰▱▱▱▱▱▱▱▱▱ 10%"
    it "" $ do
      progress 49 100 `shouldBe` "▰▰▰▰▱▱▱▱▱▱ 40%"
