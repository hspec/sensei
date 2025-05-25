module GHC.Diagnostic.UtilSpec (spec) where

import Helper

import Data.Text qualified as T

import GHC.Diagnostic.Annotated
import GHC.Diagnostic.Util

spec :: Spec
spec = do
  describe "joinMessageLines" do
    context "when a line starts with whitespace" do
      it "joins that line with the previous line" do
        joinMessageLines (T.unlines [
            "foo"
          , "  bar"
          , "    baz"
          , "foo"
          , "bar"
          , "baz"
          ]) `shouldBe` T.unlines [
            "foo bar baz"
          , "foo"
          , "bar"
          , "baz"
          ]

  describe "sortImports" do
    context "with an unqualified name" do
      it "sorts imports" do
        let
          name :: RequiredVariable
          name = "fromList"
        sortImports name [
            "Data.List.NonEmpty"
          , "Data.Map"
          , "Data.Map.Internal"
          , "Data.Map.Lazy"
          , "Data.Map.Strict"
          , "Data.Map.Strict.Internal"
          , "Data.Set"
          ] `shouldBe` [
            "Data.List.NonEmpty"
          , "Data.Map"
          , "Data.Map.Lazy"
          , "Data.Map.Strict"
          , "Data.Map.Strict.Internal"
          , "Data.Map.Internal"
          , "Data.Set"
          ]

    context "with a qualified name" do
      it "sorts imports" do
        let
          name :: RequiredVariable
          name = RequiredVariable (Qualified "Map") "fromList" Nothing
        sortImports name [
            "Data.List.NonEmpty"
          , "Data.Map"
          , "Data.Map.Internal"
          , "Data.Map.Lazy"
          , "Data.Map.Strict"
          , "Data.Map.Strict.Internal"
          , "Data.Set"
          ] `shouldBe` [
            "Data.Map"
          , "Data.Map.Lazy"
          , "Data.Map.Strict"
          , "Data.Map.Strict.Internal"
          , "Data.Map.Internal"
          , "Data.List.NonEmpty"
          , "Data.Set"
          ]
