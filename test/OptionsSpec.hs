module OptionsSpec (main, spec) where

import           Test.Hspec

import           Options

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseArgs" $ do
    it "returns longest matching list of Hspec arguments from end of given list" $ do
      parseArgs ["foo", "--bar", "-m", "FooSpec", "-a", "1000"] `shouldBe` (Run $ RunArgs ["foo", "--bar"] ["-m", "FooSpec", "-a", "1000"])

    it "assumes everything after the last '--' to be Hspec arguments" $ do
      parseArgs ["foo", "bar", "--", "foo", "baz"] `shouldBe` (Run $ RunArgs ["foo", "bar"] ["foo", "baz"])

    it "recognizes help" $ do
      parseArgs ["--help"] `shouldBe` Help
