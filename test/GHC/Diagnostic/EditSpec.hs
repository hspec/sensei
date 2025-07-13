module GHC.Diagnostic.EditSpec (spec) where

import Helper

import Data.Text qualified as T

import GHC.Diagnostic.Edit

spec :: Spec
spec = do
  describe "cut" do
    let
      start = Location 2 2
      end = Location 3 1

    it "splits the input into three pieces" do
      cut start end (T.unlines [
          "foo"
        , "bar"
        , "baz"
        ]) `shouldBe` ("foo\nb", "ar\n", "baz\n")

    context "when the input offset is non-zero" do
      it "splits the input into three pieces" do
        cut start end (T.drop 4 $ T.unlines [
            "pre"
          , "foo"
          , "bar"
          , "baz"
          ]) `shouldBe` ("foo\nb", "ar\n", "baz\n")

    context "when the end position is smaller than the start position" do
      it "splits the input into three pieces" do
        cut end start (T.unlines [
            "foo"
          , "bar"
          , "baz"
          ]) `shouldBe` ("foo\nb", "ar\n", "baz\n")

    context "when the end position is larger than the end of the input" do
      it "splits the input into three pieces" do
        cut start (Location 3 10) (T.unlines [
            "foo"
          , "bar"
          , "baz"
          ]) `shouldBe` ("foo\nb", "ar\nbaz\n", "")

  describe "replaceFirst" do
    let
      input = " - foo foo foo - "

    it "replaces the first occurrence" do
      replaceFirst "foo" "barbaz" input `shouldBe` " - barbaz foo foo - "

    context "when the input offset is non-zero" do
      it "replaces the first occurrence" do
        replaceFirst "foo" "barbaz" (T.drop 4 input) `shouldBe` "oo barbaz foo - "

    context "with the empty string" do
      it "replaces the first occurrence" do
        replaceFirst "" "foo" "barbaz" `shouldBe` "foobarbaz"
