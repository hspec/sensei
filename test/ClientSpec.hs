module ClientSpec (spec) where

import           Helper

import           HTTP
import           Client
import qualified Trigger

spec :: Spec
spec = do
  describe "client" $ do
    it "does a HTTP request via a Unix domain socket" $ do
      withTempDirectory $ \ dir -> do
        withServer dir (return (Trigger.Success, "hello")) $ do
          client dir `shouldReturn` (True, "hello")

    it "indicates failure" $ do
      withTempDirectory $ \ dir -> do
        withServer dir (return (Trigger.Failure, "hello")) $ do
          client dir `shouldReturn` (False, "hello")

    context "when server socket is missing" $ do
      it "reports error" $ do
        withTempDirectory $ \ dir -> do
          client dir `shouldReturn` (False, "could not connect to " <> fromString (socketName dir) <> "\n")
