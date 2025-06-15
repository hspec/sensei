module HIESpec (spec) where

import Data.Map qualified as Map
import Helper

import GHC.EnvironmentFile
import HIE

spec :: Spec
spec = do
  describe "reverse" $ do
    fit "reverses a list" $ do
      info <- ghcInfo
      [x] <- filter (isInfixOf "markdown-unlit") <$> listAllHieFiles info
      ref <- newIORef mempty
      loadAllPackages [x] $ modifyIORef ref
      Map.toList <$> readIORef ref `shouldReturn` [
          (":&:", ["Text.Markdown.Unlit"])
        , (":|:", ["Text.Markdown.Unlit"])
        , ("Class", ["Text.Markdown.Unlit"])
        , ("CodeBlock", ["Text.Markdown.Unlit", "Text.Markdown.Unlit", "Text.Markdown.Unlit"])
        , ("Not", ["Text.Markdown.Unlit"])
        , ("Selector", ["Text.Markdown.Unlit", "Text.Markdown.Unlit"])
        , ("codeBlockClasses", ["Text.Markdown.Unlit"])
        , ("codeBlockContent", ["Text.Markdown.Unlit"])
        , ("codeBlockStartLine", ["Text.Markdown.Unlit"])
        , ("parse", ["Text.Markdown.Unlit"])
        , ("parseSelector", ["Text.Markdown.Unlit"])
        , ("run", ["Text.Markdown.Unlit"])
        , ("unlit", ["Text.Markdown.Unlit"])
        ]
