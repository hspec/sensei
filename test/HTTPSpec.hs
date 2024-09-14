module HTTPSpec (spec) where

import           Helper

import           Test.Hspec.Wai
import qualified System.Console.ANSI as Ansi

import           HTTP
import qualified Trigger

spec :: Spec
spec = do
  describe "app" $ do
    with (return $ app $ return (Trigger.Success, withColor Green "hello")) $ do
      it "returns 200 on success" $ do
        get "/" `shouldRespondWith` fromString (withColor Green "hello")

      context "with ?color" $ do
        it "keeps terminal sequences" $ do
          get "/?color" `shouldRespondWith` fromString (withColor Green "hello")

      context "with ?color=true" $ do
        it "keeps terminal sequences" $ do
          get "/?color=true" `shouldRespondWith` fromString (withColor Green "hello")

      context "with ?color=false" $ do
        it "removes terminal sequences" $ do
          get "/?color=false" `shouldRespondWith` "hello"

      context "with an in invalid value for ?color" $ do
        it "returns status 400" $ do
          get "/?color=some%20value" `shouldRespondWith` 400 { matchBody = "invalid value for color: some%20value" }

    with (return $ app $ return (Trigger.Failure, "hello")) $ do
      it "return 500 on failure" $ do
        get "/" `shouldRespondWith` 500

  describe "stripAnsi" $ do
    it "removes ANSI color sequences" $ do
      stripAnsi ("some " <> withColor Green "colorized" <> " text") `shouldBe` "some colorized text"

    it "removes DEC private mode sequences" $ do
      stripAnsi (Ansi.hideCursorCode <> "some text" <> Ansi.showCursorCode) `shouldBe` "some text"
