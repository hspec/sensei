{-# LANGUAGE CPP #-}
module Language.Haskell.GhciWrapperSpec (main, spec) where

import           Helper

import           Language.Haskell.GhciWrapper (Interpreter)
import qualified Language.Haskell.GhciWrapper as Interpreter

main :: IO ()
main = hspec spec

withInterpreter :: [String] -> (Interpreter -> IO a) -> IO a
withInterpreter = Interpreter.withInterpreter ghciConfig

withGhci :: ((String -> IO String) -> IO a) -> IO a
withGhci action = withInterpreter [] $ action . Interpreter.eval

spec :: Spec
spec = do
  describe "evalEcho" $ do
    it "prints result to stdout" $ do
      withInterpreter [] $ \ ghci -> do
        capture (Interpreter.evalEcho ghci $ "putStr" ++ show "foo\nbar") `shouldReturn` ("foo\nbar", "foo\nbar")

  describe "eval" $ do
    it "shows literals" $ withGhci $ \ ghci -> do
      ghci "23" `shouldReturn` "23\n"

    it "shows string literals containing Unicode" $ withGhci $ \ ghci -> do
      ghci "\"λ\"" `shouldReturn` "\"\\955\"\n"

    it "evaluates simple expressions" $ withGhci $ \ ghci -> do
      ghci "23 + 42" `shouldReturn` "65\n"

    it "uses LineBuffering for stdout and stderr" $ withGhci $ \ ghci -> do
      ghci "GHC.IO.Handle.hGetBuffering System.IO.stdout" `shouldReturn` "LineBuffering\n"
      ghci "GHC.IO.Handle.hGetBuffering System.IO.stderr" `shouldReturn` "LineBuffering\n"

    it "supports let bindings" $ withGhci $ \ ghci -> do
      ghci "let x = 10" `shouldReturn` ""
      ghci "x" `shouldReturn` "10\n"

    it "allows import statements" $ withGhci $ \ ghci -> do
      ghci "import Data.Maybe" `shouldReturn` ""
      ghci "fromJust (Just 20)" `shouldReturn` "20\n"

    it "captures stdout" $ withGhci $ \ ghci -> do
      ghci "putStr \"foo\"" `shouldReturn` "foo"

    it "captures stdout (Unicode)" $ withGhci $ \ ghci -> do
      ghci "putStrLn \"λ\"" `shouldReturn` "λ\n"

    it "captures stdout (empty line)" $ withGhci $ \ ghci -> do
      ghci "putStrLn \"\"" `shouldReturn` "\n"

    it "captures stdout (multiple lines)" $ withGhci $ \ ghci -> do
      ghci "putStrLn \"foo\" >> putStrLn \"bar\" >> putStrLn \"baz\""
        `shouldReturn` "foo\nbar\nbaz\n"

    it "captures stderr" $ withGhci $ \ ghci -> do
      ghci "import System.IO" `shouldReturn` ""
      ghci "hPutStrLn stderr \"foo\"" `shouldReturn` "foo\n"

    it "captures stderr (Unicode)" $ withGhci $ \ ghci -> do
      ghci "import System.IO" `shouldReturn` ""
      ghci "hPutStrLn stderr \"λ\"" `shouldReturn` "λ\n"

    it "shows exceptions" $ withGhci $ \ ghci -> do
      ghci "import Control.Exception" `shouldReturn` ""
      ghci "throwIO DivideByZero" `shouldReturn` "*** Exception: divide by zero\n"

    it "shows exceptions (ExitCode)" $ withGhci $ \ ghci -> do
      ghci "import System.Exit" `shouldReturn` ""
      ghci "exitWith $ ExitFailure 10" `shouldReturn` "*** Exception: ExitFailure 10\n"

    it "gives an error message for identifiers that are not in scope" $ withGhci $ \ ghci -> do
      ghci "foo" >>= (`shouldSatisfy` isInfixOf "Variable not in scope: foo")

    context "with -XOverloadedStrings, -Wall and -Werror" $ do
      it "does not fail on marker expression (bug fix)" $ withGhci $ \ ghci -> do
#if __GLASGOW_HASKELL__ == 900
        _ <- ghci ":set -XOverloadedStrings -Wall -Werror"
#else
        ghci ":set -XOverloadedStrings -Wall -Werror" `shouldReturn` ""
#endif
        ghci "putStrLn \"foo\"" `shouldReturn` "foo\n"

    context "with NoImplicitPrelude" $ do
      it "works" $ withInterpreter ["-XNoImplicitPrelude"] $ \ ghci -> do
        Interpreter.eval ghci "putStrLn \"foo\"" >>= (`shouldContain` "Variable not in scope: putStrLn")
        Interpreter.eval ghci "23" `shouldReturn` "23\n"

    context "with a strange String type" $ do
      it "works" $ withGhci $ \ ghci -> do
        ghci "type String = Int" `shouldReturn` ""
        ghci "putStrLn \"foo\"" `shouldReturn` "foo\n"
