{-# LANGUAGE CPP #-}
module GHC.DiagnosticSpec (spec) where

import           Prelude hiding (span)

import           Helper hiding (diagnostic)

import           System.Process
import           System.Environment

import           Language.Haskell.GhciWrapper (lookupGhc)
import           GHC.Diagnostic

data Requirement = NoRequirement | RequireGhc912

test :: HasCallStack => FilePath -> [String] -> Maybe Action -> Spec
test name args = testWith name NoRequirement args

testWith :: HasCallStack => FilePath -> Requirement -> [String] -> Maybe Action -> Spec
testWith name requirement extraArgs action = it name $ do
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- encodeUtf8 <$> ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") json
  Just diagnostic <- return $ parse json
  when shouldRun $ do
    format diagnostic `shouldBe` err
  normalizeFileName <$> analyze diagnostic `shouldBe` action
  where
    dir :: FilePath
    dir = "test" </> "fixtures" </> name

    ghc :: [String] -> IO String
    ghc args = do
      requireGhc [9,10]
      bin <- lookupGhc <$> getEnvironment
      let
        process :: CreateProcess
        process = proc bin ("-fno-code" : args ++ extraArgs ++ [dir </> "Foo.hs"])
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

ftest :: HasCallStack => FilePath -> [String] -> Maybe Action -> Spec
ftest name args = focus . test name args

xtest :: HasCallStack => FilePath -> [String] -> Maybe Action -> Spec
xtest name args = before_ pending . test name args

_ignore :: ()
_ignore = let _ = (ftest, xtest) in ()

replace :: Location -> Location -> Text -> Action
replace start end = Replace (Span "Foo.hs" start end)

replace_ :: Location -> Location -> Text -> Maybe Action
replace_ start end = Just . replace start end

addExtension :: Text -> Maybe Action
addExtension = Just . AddExtension "Foo.hs"

spec :: Spec
spec = do
  describe "format" $ do
    test "not-in-scope" [] Nothing
    test "not-in-scope-perhaps-use" [] $ replace_ (Location 2 7) (Location 2 14) "filter"
    test "not-in-scope-perhaps-use-one-of-these" [] . Just . Choices $ map
      (replace (Location 2 7) (Location 2 11)) [
        "foldl"
      , "foldr"
      ]
    test "not-in-scope-perhaps-use-multiline" [] . Just . Choices $ map
      (replace (Location 3 7) (Location 3 11)) [
        "foldl"
      , "foldr"
      ]
    test "use-BlockArguments" [] $ addExtension "BlockArguments"
    test "use-TemplateHaskellQuotes" [] $ addExtension "TemplateHaskellQuotes"
    testWith "redundant-import" RequireGhc912 ["-Wall", "-Werror"] $ replace_ (Location 2 1) (Location 3 1) ""
    test "non-existing" [] Nothing
    test "parse-error" [] Nothing
    test "lex-error" [] Nothing
    test "multiple-error-messages" [] Nothing

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
