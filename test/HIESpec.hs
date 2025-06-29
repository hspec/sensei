module HIESpec (spec) where

import Helper hiding (lookup)
import Data.Map qualified as Map
import Control.Concurrent.Async qualified as Async

import GHC.Diagnostic (Name(..), NameSpace(..), ProvidedBy(..))
import GHC.Diagnostic.Annotated (Module(..))
import GHC.EnvironmentFile
import GHC.EnvironmentFileSpec (storePathContains)

import HIE

spec :: Spec
spec = do
  info <- runIO ghcInfo

  let
    providedBy_markdown_unlit = ProvidedBy (Module "markdown-unlit" "Text.Markdown.Unlit") Nothing
    providedBy_markdown_unlit_Selector = providedBy_markdown_unlit { type_ = Just "Selector" }
    providedBy_markdown_unlit_CodeBlock = providedBy_markdown_unlit { type_ = Just "CodeBlock" }

  describe "with" do
    it "loads dependencies asynchronously" $ whenGhc GHC_908 do
      with mempty info \ _ exports async -> do
        let lookup name = Map.lookup (Name VariableName name) <$> exports
        lookup "unlit" `shouldReturn` Nothing
        lookup "putList" `shouldReturn` Nothing
        Async.wait async
        lookup "unlit" `shouldReturn` Just [providedBy_markdown_unlit]
        lookup "putList" `shouldReturn` Just [ProvidedBy (Module "binary" "Data.Binary") (Just "Binary")]

  describe "readHieFiles" do
    it "reads exports from a list of HIE files" $ whenGhc GHC_908 do
      files <- filter (storePathContains "markdown-unlit" . (.path)) . snd <$> listAllHieFiles info
      exports <- newIORef mempty
      _ <- readHieFiles files (modifyIORef' exports)
      Map.toList <$> readIORef exports `shouldReturn` [
          (Name VariableName ":&:", [providedBy_markdown_unlit_Selector])
        , (Name VariableName ":|:", [providedBy_markdown_unlit_Selector])
        , (Name VariableName "Class", [providedBy_markdown_unlit_Selector])
        , (Name VariableName "CodeBlock", [providedBy_markdown_unlit_CodeBlock])
        , (Name VariableName "Not", [providedBy_markdown_unlit_Selector])
        , (Name VariableName "codeBlockClasses", [providedBy_markdown_unlit_CodeBlock])
        , (Name VariableName "codeBlockContent", [providedBy_markdown_unlit_CodeBlock])
        , (Name VariableName "codeBlockStartLine", [providedBy_markdown_unlit_CodeBlock])
        , (Name VariableName "parse", [providedBy_markdown_unlit])
        , (Name VariableName "parseSelector", [providedBy_markdown_unlit])
        , (Name VariableName "run", [providedBy_markdown_unlit])
        , (Name VariableName "unlit", [providedBy_markdown_unlit])
        , (Name TypeName "CodeBlock", [providedBy_markdown_unlit_CodeBlock])
        , (Name TypeName "Selector", [providedBy_markdown_unlit_Selector])
        ]

    context "with a missing HIE file" do
      it "reports an error" do
        withTempDirectory \ dir -> do
          let file = dir </> "Foo.hie"
          readHieFiles [HieFilePath "main" file] mempty <&> (.errors) `shouldReturn` [unlines [
              "Reading " <> file <> " failed:"
            , "  " <> file <> ": withBinaryFile: does not exist (No such file or directory)"
            ]]

    context "with a corrupted HIE file" do
      it "reports an error" do
        withTempDirectory \ dir -> do
          let file = dir </> "Foo.hie"
          writeFile file ""
          readHieFiles [HieFilePath "main" file] mempty <&> (.errors) `shouldReturn` [unlines [
              "Reading " <> file <> " failed:"
            , "  Data.Binary.getPrim: end of file"
            ]]
