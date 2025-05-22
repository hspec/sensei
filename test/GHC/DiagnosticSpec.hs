{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module GHC.DiagnosticSpec (spec) where

import           Prelude hiding (span)

import           Helper hiding (diagnostic)
import           Test.Hspec.Expectations.Contrib qualified as Hspec
import           Text.RawString.QQ (r, rQ)

import           System.Process
import           System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Language.Haskell.GhciWrapper (lookupGhc)
import           GHC.Diagnostic

data Requirement = NoRequirement | RequireGhc912

test, ftest, xtest :: HasCallStack => FilePath -> [String] -> String -> Maybe Action -> Spec

test name = testWith name NoRequirement

ftest name args code = focus . test name args code

xtest name args code = before_ pending . test name args code

_ignore :: ()
_ignore = let _ = (ftest, xtest) in ()

normalizeGhcVersion :: String -> String
normalizeGhcVersion = T.unpack . T.replace __GLASGOW_HASKELL_FULL_VERSION__ "9.10.0" . T.pack

testWith :: HasCallStack => FilePath -> Requirement -> [String] -> String -> Maybe Action -> Spec
testWith name requirement extraArgs (unindent -> code) action = it name $ do
  unless (T.null code) do
    ensureFile src $ T.encodeUtf8 code
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") (encodeUtf8 $ normalizeGhcVersion json)
  Hspec.annotate (separator <> err <> separator) do
    Just diagnostic <- return . parse $ encodeUtf8 json
    when shouldRun $ do
      format diagnostic `shouldBe` err
    normalizeFileName <$> analyze diagnostic `shouldBe` action
  where
    separator :: String
    separator = replicate 30 '*' <> "\n"

    dir :: FilePath
    dir = "test" </> "fixtures" </> name

    src :: FilePath
    src = dir </> "Foo.hs"

    ghc :: [String] -> IO String
    ghc args = do
      requireGhc [9,10]
      bin <- lookupGhc <$> getEnvironment
      let
        process :: CreateProcess
        process = proc bin ("-fno-code" : args ++ extraArgs ++ [src])
      (_, _, err) <- readCreateProcessWithExitCode process ""
      return err

    translate :: String -> String
    translate = map \ case
      '‘' -> '`'
      '’' -> '\''
      c -> c

    shouldRun :: Bool
    shouldRun = case requirement of
      NoRequirement -> True
      RequireGhc912 ->
#if __GLASGOW_HASKELL__ < 912
        False
#else
        True
#endif

normalizeFileName :: Action -> Action
normalizeFileName = \ case
  Choices choices -> Choices $ map normalizeFileName choices
  AddExtension _ name -> AddExtension "Foo.hs" name
  Replace span substitute -> Replace span {file = "Foo.hs"} substitute

unindent :: String -> Text
unindent (T.pack >>> T.dropWhileEnd isSpace >>> T.lines -> input) = go input
  where
    go :: [Text] -> Text
    go = map (T.drop $ indentation input) >>> T.unlines >>> T.dropWhile isSpace

    indentation :: [Text] -> Int
    indentation = dropEmptyLines >>> map (T.length . T.takeWhile isSpace) >>> maximum

    dropEmptyLines :: [Text] -> [Text]
    dropEmptyLines = filter (not . T.all isSpace)

replace :: Location -> Location -> Text -> Action
replace start end = Replace (Span "Foo.hs" start end)

replace_ :: Location -> Location -> Text -> Maybe Action
replace_ start end = Just . replace start end

addExtension :: Text -> Maybe Action
addExtension = Just . AddExtension "Foo.hs"

spec :: Spec
spec = do
  describe "format" $ do
    test "not-in-scope" [] [r|
      module Foo where
      foo = c2w
      |] Nothing

    test "not-in-scope-perhaps-use" [] [r|
      module Foo where
      foo = filter_
      |] $ replace_ (Location 2 7) (Location 2 14) "filter"

    test "not-in-scope-perhaps-use-one-of-these" [] [r|
      module Foo where
      foo = fold
      |] $ Just . Choices $ map
      (replace (Location 2 7) (Location 2 11)) [
        "foldl"
      , "foldr"
      ]

    test "not-in-scope-perhaps-use-multiline" [] [r|
      module Foo where
      import Data.List
      foo = fold
      |] $ Just . Choices $ map
      (replace (Location 3 7) (Location 3 11)) [
        "foldl"
      , "foldr"
      ]

    test "use-BlockArguments" [] [r|
      {-# LANGUAGE NoBlockArguments #-}
      module Foo where

      foo :: IO ()
      foo = id do return ()
      |] $ addExtension "BlockArguments"

    test "use-TemplateHaskellQuotes" [] [rQ|
      module Foo where
      foo = [|23|~]
      |] $ addExtension "TemplateHaskellQuotes"

    testWith "redundant-import" RequireGhc912 ["-Wall", "-Werror"] [r|
      module Foo where
      import Data.Maybe
      |] $ replace_ (Location 2 1) (Location 3 1) ""

    test "non-existing" [] [r|
      |] Nothing

    test "parse-error" [] [r|
      module Foo where

      data foo
      |] Nothing

    test "lex-error" [] [r|
      module Foo where

      foo = "bar
      |] Nothing

    test "multiple-error-messages" [] [r|
      module Foo where

      foo = "foo" + 23
      |] Nothing

  describe "extractIdentifiers" $ do
    it "extracts identifiers" $ do
      extractIdentifiers ".. `foldl' ..., `foldr' .." `shouldBe` ["foldl", "foldr"]

  describe "applyReplace" $ do
    it "replaces a given source span with a substitute" $ do
      applyReplace (Location 2 7) (Location 2 14) "filter" [
          "module Foo where"
        , "foo = filter_ p xs"
        ] `shouldBe` [
          "module Foo where"
        , "foo = filter p xs"
        ]

    it "correctly handles source spans that span over multiple lines" $ do
      applyReplace (Location 2 8) (Location 3 7) "Ya" [
          "module Foo where"
        , "import Data.Maybe"
        , "foo = bar"
        , "one = two"
        ] `shouldBe` [
          "module Foo where"
        , "import Yabar"
        , "one = two"
        ]

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
