-- FIXME: check performance impact
{-# LANGUAGE BlockArguments #-}
module GHC.DiagnosticSpec (spec) where

import           Helper hiding (diagnostic)

import           System.Process

import           GHC.Diagnostic

test :: HasCallStack => FilePath -> Spec
test name = it name $ do
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  Just diagnostic <- parse . encodeUtf8 <$> ghc ["-fdiagnostics-as-json"]
  decodeUtf8 (format diagnostic) `shouldBe` err
  where
    ghc :: [String] -> IO String
    ghc args = do
      let
        process :: CreateProcess
        process = proc "ghc" (args ++ ["test/assets" </> name </> "Foo.hs"])
      (_, _, err) <- readCreateProcessWithExitCode process ""
      return err

    translate :: String -> String
    translate = map \ case
      '‘' -> '`'
      '’' -> '\''
      -- '•' -> '*'
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
