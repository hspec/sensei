module OptionsSpec (spec) where

import           Helper

import           Options

spec :: Spec
spec = do
  describe "splitArgs" do
    it "passes command-line arguments that look like Hspec options to Hspec" do
      splitArgs ["foo", "-m", "FooSpec", "--bar", "-a", "1000"] `shouldBe`
        (["foo", "--bar"], ["-m", "FooSpec", "-a", "1000"])

    it "passes -f options to GHC" do
      splitArgs ["--format", "checks", "-fno-diagnostics-as-json"] `shouldBe`
        (["-fno-diagnostics-as-json"], ["--format", "checks"])

    it "assumes everything after the last '--' to be Hspec options" do
      splitArgs ["foo", "bar", "--", "foo", "baz"] `shouldBe` (["foo", "bar"], ["foo", "baz"])

    context "with -p" do
      it "recognizes -p as an Hspec option" do
        splitArgs ["-p"] `shouldBe` ([], ["-p"])

      it "recognizes -pN as an Hspec option" do
        splitArgs ["-p20"] `shouldBe` ([], ["-p20"])

      it "recognizes -packageNAME as a GHC option" do
        splitArgs ["-packagebase"] `shouldBe` (["-packagebase"], [])
