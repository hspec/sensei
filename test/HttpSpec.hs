{-# LANGUAGE OverloadedStrings #-}
module HttpSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Http

spec :: Spec
spec = do
  describe "app" $ do
    with (return $ app $ return (True, "hello")) $ do
      it "returns 200 on success" $ do
        get "/" `shouldRespondWith` 200

    with (return $ app $ return (False, "hello")) $ do
      it "return 412 on failure" $ do
        get "/" `shouldRespondWith` 412
