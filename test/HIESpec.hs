module HIESpec (spec) where

import Helper
import Data.Map qualified as Map
import Control.Concurrent.Async qualified as Async

import GHC.EnvironmentFile
import GHC.EnvironmentFileSpec (storePathContains)

import HIE

spec :: Spec
spec = do
  info <- runIO ghcInfo

  describe "with" do
    it "loads dependencies asynchronously" $ whenGhc GHC_908 do
      with mempty info \ _ exports async -> do
        Map.lookup "unlit" <$> exports `shouldReturn` Nothing
        Map.lookup "putList" <$> exports `shouldReturn` Nothing
        Async.wait async
        Map.lookup "unlit" <$> exports `shouldReturn` Just ["Text.Markdown.Unlit"]
        Map.lookup "putList" <$> exports `shouldReturn` Just ["Data.Binary"]

  describe "readHieFiles" do
    it "reads exports from a list of HIE files" $ whenGhc GHC_908 do
      files <- filter (storePathContains "markdown-unlit" . unPath) . snd <$> listAllHieFiles info
      exports <- newIORef mempty
      _ <- readHieFiles files (modifyIORef' exports)
      Map.toList <$> readIORef exports `shouldReturn` [
          (":&:", ["Text.Markdown.Unlit"])
        , (":|:", ["Text.Markdown.Unlit"])
        , ("Class", ["Text.Markdown.Unlit"])
        , ("CodeBlock", ["Text.Markdown.Unlit", "Text.Markdown.Unlit"])
        , ("Not", ["Text.Markdown.Unlit"])
        , ("Selector", ["Text.Markdown.Unlit"])
        , ("codeBlockClasses", ["Text.Markdown.Unlit"])
        , ("codeBlockContent", ["Text.Markdown.Unlit"])
        , ("codeBlockStartLine", ["Text.Markdown.Unlit"])
        , ("parse", ["Text.Markdown.Unlit"])
        , ("parseSelector", ["Text.Markdown.Unlit"])
        , ("run", ["Text.Markdown.Unlit"])
        , ("unlit", ["Text.Markdown.Unlit"])
        ]

    context "with a missing HIE file" do
      it "reports an error" do
        withTempDirectory \ dir -> do
          let file = dir </> "Foo.hie"
          readHieFiles [Path file] mempty <&> (.errors) `shouldReturn` [unlines [
              "Reading " <> file <> " failed:"
            , "  " <> file <> ": withBinaryFile: does not exist (No such file or directory)"
            ]]

    context "with a corrupted HIE file" do
      it "reports an error" do
        withTempDirectory \ dir -> do
          let file = dir </> "Foo.hie"
          writeFile file ""
          readHieFiles [Path file] mempty <&> (.errors) `shouldReturn` [unlines [
              "Reading " <> file <> " failed:"
            , "  Data.Binary.getPrim: end of file"
            ]]
