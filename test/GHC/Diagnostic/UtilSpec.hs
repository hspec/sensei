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
        sortImports Unqualified (Name VariableName "foo") id [
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
          , "Data.Map.Internal"
          , "Data.Map.Strict.Internal"
          , "Data.Set"
          ]

    context "with a qualified name" do
      it "sorts imports" do
        sortImports "Map" (Name VariableName "foo") id [
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
          , "Data.Map.Internal"
          , "Data.Map.Strict.Internal"
          , "Data.List.NonEmpty"
          , "Data.Set"
          ]

    context "when the components of a module A are a subset of the components of an other module B" do
      it "puts module A before module B" do
        sortImports "Map" (Name VariableName "foo") id [
            "Data.Text.Internal.Lazy.Search"
          , "Data.Text.Internal.Search"
          ] `shouldBe` [
            "Data.Text.Internal.Search"
          , "Data.Text.Internal.Lazy.Search"
          ]

    context "when importing types" do
      context "when the type name is a module name component" do
        it "prioritizes that module" do
          sortImports Unqualified (Name TypeName "User") id [
              "Models.Product"
            , "Models.User"
            ] `shouldBe` [
              "Models.User"
            , "Models.Product"
            ]
