module GHC.DiagnosticSpec (spec) where

import           Prelude hiding (span)

import           Helper hiding (diagnostic)

import           System.Process
import           System.Environment
import           Data.Text (Text)

import           Language.Haskell.GhciWrapper (lookupGhc)
import           GHC.Diagnostic

test :: HasCallStack => FilePath -> Maybe Action -> Spec
test name action = it name $ do
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- encodeUtf8 <$> ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") json
  Just diagnostic <- return $ parse json
  format diagnostic `shouldBe` err
  normalizeFileName <$> analyze diagnostic `shouldBe` action
  where
    dir :: FilePath
    dir = "test" </> "assets" </> name

    ghc :: [String] -> IO String
    ghc args = do
      requireGhc [9,10]
      bin <- lookupGhc <$> getEnvironment
      let
        process :: CreateProcess
        process = proc bin ("-fno-code" : args ++ [dir </> "Foo.hs"])
      (_, _, err) <- readCreateProcessWithExitCode process ""
      return err

    translate :: String -> String
    translate = map \ case
      '‘' -> '`'
      '’' -> '\''
      c -> c

normalizeFileName :: Action -> Action
normalizeFileName = \ case
  AddExtension _ name -> AddExtension "Foo.hs" name
  Replace span substitute -> Replace span {file = "Foo.hs"} substitute

ftest :: HasCallStack => FilePath -> Maybe Action -> Spec
ftest name = focus . test name

_ignore :: HasCallStack => FilePath -> Maybe Action -> Spec
_ignore = ftest

replace :: Location -> Location -> Text -> Maybe Action
replace start end = Just . Replace (Span "Foo.hs" start end)

addExtension :: Text -> Maybe Action
addExtension = Just . AddExtension "Foo.hs"

spec :: Spec
spec = do
  describe "format" $ do
    test "not-in-scope" Nothing
    test "not-in-scope-perhaps-use" $ replace (Location 2 7) (Location 2 14) "filter"
    test "not-in-scope-perhaps-use-one-of-these" $ replace (Location 2 7) (Location 2 11) "foldl"
    test "not-in-scope-perhaps-use-multiline" $ replace (Location 3 7) (Location 3 11) "foldl"
    test "use-BlockArguments" $ addExtension "BlockArguments"
    test "use-TemplateHaskellQuotes" $ addExtension "TemplateHaskellQuotes"
    test "non-existing" Nothing
    test "parse-error" Nothing
    test "lex-error" Nothing
    test "multiple-error-messages" Nothing

  describe "applyReplace" $ do
    it "replaces a given source span with a substitute" $ do
      applyReplace (Location 2 7) (Location 2 14) "filter" [
          "module Foo where"
        , "foo = filter_ p xs"
        ] `shouldBe` [
          "module Foo where"
        , "foo = filter p xs"
        ]
