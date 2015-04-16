module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      foo [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]
