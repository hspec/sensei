module OptionsSpec (main, spec) where

import           Test.Hspec

import           Options

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "splitArgs" $ do
    it "returns longest matching list of Hspec arguments from end of given list" $ do
      splitArgs ["foo", "--bar", "-m", "FooSpec", "-a", "1000"] `shouldBe` (["foo", "--bar"], ["-m", "FooSpec", "-a", "1000"])

    it "assumes everything after the last '--' to be Hspec arguments" $ do
      splitArgs ["foo", "bar", "--", "foo", "baz"] `shouldBe` (["foo", "bar"], ["foo", "baz"])
