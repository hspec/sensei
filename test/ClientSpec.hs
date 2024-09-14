module ClientSpec (spec) where

import           Helper

import qualified HTTP
import           HTTP.Util (socketName)
import           Client
import qualified Trigger

withSuccess :: (FilePath -> IO a) -> IO a
withSuccess = withApp Trigger.Success (withColor Green "success")

withFailure :: (FilePath -> IO a) -> IO a
withFailure = withApp Trigger.Failure (withColor Red "failure")

withApp :: Trigger.Result -> String -> (FilePath -> IO a) -> IO a
withApp result text action = do
  withTempDirectory $ \ dir -> do
    HTTP.withApp (appConfig dir) { getLastResult = return (result, text, []) } $ do
      action dir

spec :: Spec
spec = do
  describe "client" $ do
    it "accepts --color" $ do
      withSuccess $ \ dir -> do
        client return dir ["--color"] `shouldReturn` (True, fromString $ withColor Green "success")

    it "accepts --no-color" $ do
      withSuccess $ \ dir -> do
        client return dir ["--no-color"] `shouldReturn` (True, "success")

    it "indicates failure" $ do
      withFailure $ \ dir -> do
        client return dir ["--color"] `shouldReturn` (False, fromString $ withColor Red "failure")

    context "when server socket is missing" $ do
      it "reports error" $ do
        withTempDirectory $ \ dir -> do
          client return dir [] `shouldReturn` (False, "could not connect to " <> fromString (socketName dir) <> "\n")

    context "with --vim-config" $ do
      it "returns a path to a Vim support file" $ do
        client return undefined ["--vim-config"] `shouldReturn` (True, "vim/sensei.vim")
