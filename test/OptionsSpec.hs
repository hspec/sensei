module OptionsSpec (spec) where

import           Helper

import           Options

spec :: Spec
spec = do
  describe "splitArgs" $ do
    it "" $ do
      splitArgs ["-fno-diagnostics-as-json", "-fchecks"] `shouldBe` (["-fno-diagnostics-as-json"], ["-fchecks"])

    it "" $ do
      splitArgs ["-fdiagnostics-color=never"] `shouldBe` (["-fdiagnostics-color=never"], [])


    it "returns longest matching list of Hspec options from end of given list" $ do
      splitArgs ["foo", "--bar", "-m", "FooSpec", "-a", "1000"] `shouldBe` (["foo", "--bar"], ["-m", "FooSpec", "-a", "1000"])

    it "assumes everything after the last '--' to be Hspec options" $ do
      splitArgs ["foo", "bar", "--", "foo", "baz"] `shouldBe` (["foo", "bar"], ["foo", "baz"])

    context "with -p" $ do
      it "recognizes -p as an Hspec option" $ do
        splitArgs ["-p"] `shouldBe` ([], ["-p"])

      it "recognizes -pN as an Hspec option" $ do
        splitArgs ["-p20"] `shouldBe` ([], ["-p20"])

      it "recognizes -packageNAME as a GHC option" $ do
        splitArgs ["-packagebase"] `shouldBe` (["-packagebase"], [])
