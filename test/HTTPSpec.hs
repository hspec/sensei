{-# LANGUAGE OverloadedStrings #-}
module HTTPSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           HTTP

spec :: Spec
spec = do
  describe "app" $ do
    with (return $ app $ return (True, "hello")) $ do
      it "returns 200 on success" $ do
        get "/" `shouldRespondWith` 200

    with (return $ app $ return (False, "hello")) $ do
      it "return 500 on failure" $ do
        get "/" `shouldRespondWith` 500
