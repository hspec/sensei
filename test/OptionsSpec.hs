module OptionsSpec (main, spec) where

import           Test.Hspec
import           System.Environment.Blank

import           Options

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getWatchMode" $ do
    context "without SENSEI_WATCH_MODE" $ do
      it "returns WatchHaskell" $ do
        unsetEnv "SENSEI_WATCH_MODE"
        getWatchMode `shouldReturn` WatchHaskell

    context "when SENSEI_WATCH_MODE=" $ do
      it "returns WatchHaskell" $ do
        setEnv "SENSEI_WATCH_MODE" "" True
        getWatchMode `shouldReturn` WatchHaskell

    context "when SENSEI_WATCH_MODE=haskell" $ do
      it "returns WatchHaskell" $ do
        setEnv "SENSEI_WATCH_MODE" "haskell" True
        getWatchMode `shouldReturn` WatchHaskell

    context "when SENSEI_WATCH_MODE=HASKELL" $ do
      it "returns WatchHaskell" $ do
        setEnv "SENSEI_WATCH_MODE" "HASKELL" True
        getWatchMode `shouldReturn` WatchHaskell

    context "when SENSEI_WATCH_MODE=all" $ do
      it "returns WatchAll" $ do
        setEnv "SENSEI_WATCH_MODE" "all" True
        getWatchMode `shouldReturn` WatchAll

    context "when SENSEI_WATCH_MODE=unknown" $ do
      it "fails" $ do
        setEnv "SENSEI_WATCH_MODE" "unknown" True
        getWatchMode `shouldThrow` errorCall "SENSEI_WATCH_MODE: unknown value \"unknown\""

  describe "splitArgs" $ do
    it "returns longest matching list of Hspec arguments from end of given list" $ do
      splitArgs ["foo", "--bar", "-m", "FooSpec", "-a", "1000"] `shouldBe` (["foo", "--bar"], ["-m", "FooSpec", "-a", "1000"])

    it "assumes everything after the last '--' to be Hspec arguments" $ do
      splitArgs ["foo", "bar", "--", "foo", "baz"] `shouldBe` (["foo", "bar"], ["foo", "baz"])
