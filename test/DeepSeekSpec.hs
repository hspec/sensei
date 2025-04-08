{-# LANGUAGE NoImplicitPrelude #-}
module DeepSeekSpec (spec) where

import           Helper hiding (diagnostic)
import qualified Helper

import qualified Data.Text as Text

import qualified Builder
import           Sensei.API
import           DeepSeek hiding (formatSpan)
import qualified DeepSeek

spec :: Spec
spec = do
  describe "createPrompt" $ do
    let
      diagnostic :: Diagnostic
      diagnostic = (Helper.diagnostic Error) { span = Just span, message = ["some error"] }

      instructions :: Instructions
      instructions = Instructions span "some instructions"

      span :: Span
      span = Span {
        file = "Foo.hs"
      , start = Location 2 1
      , end = Location 2 1
      }

      withSourceFile :: (String -> IO a) -> IO a
      withSourceFile action = withTempDirectory \ dir -> do
        writeFile (dir </> "Foo.hs") $ unlines [
            "module Foo where"
          , "foo = 23"
          ]
        action dir

    around withSourceFile do
      context "with a GHC diagnostics message" do
        let input = This diagnostic

        it "creates a prompt" \ dir -> do
          createPrompt dir span input `shouldReturn` Text.unlines [
              "Given a GHC diagnostics message, please suggest a fix for the corresponding Haskell code."
            , ""
            , "Produce your answer as a unified diff so that it can be applied with the `patch` program."
            , "Don't provide explanations."
            , ""
            , "Enclose your answer in:"
            , ""
            , "```diff"
            , "--- Foo.hs"
            , "+++ Foo.hs"
            , "..."
            , "```"
            , ""
            , "(where ... is the placeholder for your answer)"
            , ""
            , "The GHC diagnostics message:"
            , ""
            , "```console"
            , "Foo.hs:2:1: error: some error"
            , "```"
            , ""
            , "The corresponding Haskell code:"
            , ""
            , "```haskell"
            , "module Foo where"
            , "foo = 23"
            , ""
            , "```"
            ]

      context "with programmer instructions" do
        let input = That instructions

        it "creates a prompt" \ dir -> do
          createPrompt dir span input `shouldReturn` Text.unlines [
              "Given instructions provided by a programmer, please suggest a fix for the corresponding Haskell code."
            , ""
            , "Produce your answer as a unified diff so that it can be applied with the `patch` program."
            , "Don't provide explanations."
            , ""
            , "Enclose your answer in:"
            , ""
            , "```diff"
            , "--- Foo.hs"
            , "+++ Foo.hs"
            , "..."
            , "```"
            , ""
            , "(where ... is the placeholder for your answer)"
            , ""
            , "The instructions provided by the programmer: some instructions"
            , ""
            , "The programmer is currently focusing on: Foo.hs:2:1"
            , ""
            , "The corresponding Haskell code:"
            , ""
            , "```haskell"
            , "module Foo where"
            , "foo = 23"
            , ""
            , "```"
            ]

      context "with a GHC diagnostics message and programmer instructions" do
        let input = These diagnostic instructions

        it "creates a prompt" \ dir -> do
          createPrompt dir span input `shouldReturn` Text.unlines [
              "Given a GHC diagnostics message and instructions provided by a programmer, please suggest a fix for the corresponding Haskell code."
            , ""
            , "Produce your answer as a unified diff so that it can be applied with the `patch` program."
            , "Don't provide explanations."
            , ""
            , "Enclose your answer in:"
            , ""
            , "```diff"
            , "--- Foo.hs"
            , "+++ Foo.hs"
            , "..."
            , "```"
            , ""
            , "(where ... is the placeholder for your answer)"
            , ""
            , "The GHC diagnostics message:"
            , ""
            , "```console"
            , "Foo.hs:2:1: error: some error"
            , "```"
            , ""
            , "The instructions provided by the programmer: some instructions"
            , ""
            , "The programmer is currently focusing on: Foo.hs:2:1"
            , ""
            , "The corresponding Haskell code:"
            , ""
            , "```haskell"
            , "module Foo where"
            , "foo = 23"
            , ""
            , "```"
            ]

  describe "formatSpan" $ do
    let
      formatSpan :: Span -> Text
      formatSpan = Builder.toText . DeepSeek.formatSpan

    it "formats a location" $ do
      formatSpan (Span "Foo.hs" (Location 23 42) (Location 23 42)) `shouldBe` "Foo.hs:23:42"

    it "formats a single-line span" $ do
      formatSpan (Span "Foo.hs" (Location 23 42) (Location 23 65)) `shouldBe` "Foo.hs:23:42-65"

    it "formats a multi-line span" $ do
      formatSpan (Span "Foo.hs" (Location 23 1) (Location 42 1)) `shouldBe` "Foo.hs:(23,1-42,1)"

  describe "extractPatch" $ do
    it "extracts patch" $ do
      let
        span :: Span
        span = Span {
          file = "src/DeepSeek/Types.hs"
        , start = Location 0 0
        , end = Location 0 0
        }

        input :: String
        input = unlines [
            "```diff"
          , "--- a/src/DeepSeek/Types.hs"
          , "+++ b/src/DeepSeek/Types.hs"
          , "@@ -11,7 +11,7 @@"
          , " import GHC.Generics (Generic)"
          , " import Data.Aeson (FromJSON, ToJSON)"
          , " import Data.Text (Text)"
          , "-import Data.Time.Clock.POSIX (POSIXTime)"
          , "+import Data.Time.Clock.POSIX ()"
          , ""
          , " data Request = Request {"
          , "   model :: String"
          , "```"
          ]
      extractPatch span input `shouldBe` Just Patch {
          strip = 1
        , diff = unlines [
            "--- a/src/DeepSeek/Types.hs"
          , "+++ b/src/DeepSeek/Types.hs"
          , "@@ -11,7 +11,7 @@"
          , " import GHC.Generics (Generic)"
          , " import Data.Aeson (FromJSON, ToJSON)"
          , " import Data.Text (Text)"
          , "-import Data.Time.Clock.POSIX (POSIXTime)"
          , "+import Data.Time.Clock.POSIX ()"
          , ""
          , " data Request = Request {"
          , "   model :: String"
          ]
        }
