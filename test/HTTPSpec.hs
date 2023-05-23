module HTTPSpec (spec) where

import           Helper

import           Test.Hspec.Wai

import           HTTP
import qualified Trigger

spec :: Spec
spec = do
  describe "app" $ do
    with (return $ app $ return (Trigger.Success, "hello")) $ do
      it "returns 200 on success" $ do
        get "/" `shouldRespondWith` 200

    with (return $ app $ return (Trigger.Failure, "hello")) $ do
      it "return 500 on failure" $ do
        get "/" `shouldRespondWith` 500
