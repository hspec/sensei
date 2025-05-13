module BuilderSpec (spec) where

import           Helper

import           Builder

spec :: Spec
spec = do
  describe "unlines" do
    it "joins lines, appending newlines" do
      let
        input :: [Builder]
        input = ["foo", "bar", "baz"]
      Builder.toText (Builder.unlines input) `shouldBe` "foo\nbar\nbaz\n"

    context "with the empty string" do
      it "returns the empty string" do
        Builder.toText (Builder.unlines []) `shouldBe` ""
