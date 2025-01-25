{-# LANGUAGE BlockArguments #-}
module GHC.DiagnosticSpec (spec) where

import           Helper hiding (diagnostic)

import           System.Process

import           System.Environment
import           Language.Haskell.GhciWrapper (lookupGhc)

import           GHC.Diagnostic

test :: HasCallStack => FilePath -> Spec
test name = it name $ do
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- encodeUtf8 <$> ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") json
  Just diagnostic <- return $ parse json
  decodeUtf8 (format diagnostic) `shouldBe` err
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

ftest :: HasCallStack => FilePath -> Spec
ftest = focus . test

_ignore :: HasCallStack => FilePath -> Spec
_ignore = ftest

spec :: Spec
spec = do
  describe "format" $ do
    test "variable-not-in-scope"
    test "variable-not-in-scope-perhaps-use"
    test "use-BlockArguments"
    test "non-existing"
    test "parse-error"
    test "lex-error"
    test "multiple-error-messages"
