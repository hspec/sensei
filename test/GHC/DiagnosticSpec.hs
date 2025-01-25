{-# LANGUAGE BlockArguments #-}
module GHC.DiagnosticSpec (spec) where

import           Helper hiding (diagnostic)

import           System.Process

import           System.Environment
import           Language.Haskell.GhciWrapper (lookupGhc)

import           GHC.Diagnostic

test :: HasCallStack => FilePath -> Maybe Action -> Spec
test name edit = it name $ do
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- encodeUtf8 <$> ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") json
  Just diagnostic <- return $ parse json
  decodeUtf8 (format diagnostic) `shouldBe` err
  analyze diagnostic `shouldBe` edit
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

ftest :: HasCallStack => FilePath -> Maybe Action -> Spec
ftest name = focus . test name

_ignore :: HasCallStack => FilePath -> Maybe Action -> Spec
_ignore = ftest

spec :: Spec
spec = do
  describe "format" $ do
    test "variable-not-in-scope" Nothing
    test "variable-not-in-scope-perhaps-use" Nothing
    test "use-BlockArguments" (Just $ AddExtension "test/assets/use-BlockArguments/Foo.hs" "BlockArguments")
    test "use-TemplateHaskellQuotes" (Just $ AddExtension "test/assets/use-TemplateHaskellQuotes/Foo.hs" "TemplateHaskellQuotes")
    test "non-existing" Nothing
    test "parse-error" Nothing
    test "lex-error" Nothing
    test "multiple-error-messages" Nothing
