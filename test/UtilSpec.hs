{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (spec) where

import           Test.Hspec

import           Util

spec :: Spec
spec = do
  describe "isBoring" $ do
    it "ignores files in .git/" $ do
      isBoring "/foo/bar/.git/baz/foo.txt" `shouldBe` True

    it "ignores files in dist/" $ do
      isBoring "/foo/bar/dist/baz/foo.txt" `shouldBe` True
