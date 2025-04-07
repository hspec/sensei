{-# LANGUAGE CPP #-}
module Language.Haskell.GhciWrapperSpec (main, spec) where

import           Helper hiding (diagnostic)
import           Util
import qualified Data.ByteString.Char8 as ByteString

import           Language.Haskell.GhciWrapper (Config(..), Interpreter(..), ReloadStatus(..), Extract(..))
import qualified Language.Haskell.GhciWrapper as Interpreter

main :: IO ()
main = hspec spec

withInterpreter :: [String] -> (Interpreter -> IO a) -> IO a
withInterpreter = Interpreter.withInterpreter ghciConfig []

withGhci :: ((String -> IO String) -> IO a) -> IO a
withGhci action = withInterpreter [] $ action . Interpreter.eval

extractNothing :: Extract ()
extractNothing = Extract {
  isPartialMessage = const False
, parseMessage = undefined
}

spec :: Spec
spec = do
  describe "withInterpreter" $ do
    context "on shutdown" $ do
      it "drains `stdout` of the `ghci` process" $ do
        result <- withSpy $ \ spy -> do
          Interpreter.withInterpreter ghciConfig {configEcho = spy} [] [] $ \ _ghci -> do
            pass
        last (ByteString.lines $ mconcat result) `shouldBe` "Leaving GHCi."

    context "when .ghci is writable by others" $ do
      let
        with :: FilePath -> Config -> IO String
        with dir config = do
          let dotGhci = dir </> ".ghci"
          writeFile dotGhci ""
          callProcess "chmod" ["go+w", dotGhci]
          Interpreter.withInterpreter config { configWorkingDirectory = Just dir } [] [] $ \ ghci -> Interpreter.eval ghci "23"

      context "when configIgnoreDotGhci is False" $ do
        it "terminates with an error message" $ do
          withTempDirectory $ \ dir -> do
            let dotGhci = dir </> ".ghci"
            with dir ghciConfig { configIgnoreDotGhci = False } `shouldThrow` (== (ErrorCall . unlines) [
                dotGhci <> " is writable by others, you can fix this with:"
              , ""
              , "    chmod go-w " <> dotGhci <> " ."
              , ""
              ])

      context "when configIgnoreDotGhci is True" $ do
        it "does not terminate" $ do
          withTempDirectory $ \ dir -> do
            with dir ghciConfig { configIgnoreDotGhci = True } `shouldReturn` "23\n"

  describe "evalVerbose" $ do
    it "echos result" $ do
      fmap mconcat . withSpy $ \ spy -> do
        withInterpreter [] $ \ ghci -> do
          Interpreter.evalVerbose extractNothing ghci {echo = spy} "23" `shouldReturn` ("23\n", [])
      `shouldReturn` "23\n"

  describe "eval" $ do
    it "does not echo result" $ do
      fmap mconcat . withSpy $ \ spy -> do
        withInterpreter [] $ \ ghci -> do
          Interpreter.eval ghci {echo = spy} "23" `shouldReturn` "23\n"
      `shouldReturn` ""

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
      expected <- ifGhc GHC_912 <&> \ case
        False -> requiredFor GHC_910 "*** Exception: divide by zero\n"
        True -> unlines ["*** Exception: divide by zero"
          , ""
          , "HasCallStack backtrace:"
          , "  throwIO, called at <interactive>:5:1 in interactive:Ghci8"
          , ""
          ]
      ghci "import Control.Exception" `shouldReturn` ""
      ghci "throwIO DivideByZero" `shouldReturn` expected

    it "shows exceptions (ExitCode)" $ withGhci $ \ ghci -> do
      ghci "import System.Exit" `shouldReturn` ""
      ghci "exitWith $ ExitFailure 10" `shouldReturn` "*** Exception: ExitFailure 10\n"

    it "gives an error message for identifiers that are not in scope" $ withGhci $ \ ghci -> do
      ghci "foo" >>= (`shouldContain` "Variable not in scope: foo")

    context "with -XNoImplicitPrelude" $ do
      it "works" $ withInterpreter ["-XNoImplicitPrelude"] $ \ ghci -> do
        normalizeTypeSignatures <$> Interpreter.eval ghci "putStrLn \"foo\"" >>= (`shouldContain` "Variable not in scope: putStrLn")
        Interpreter.eval ghci "23" `shouldReturn` "23\n"

  describe "reload" do
    let
      withModule :: (FilePath -> IO a) -> IO a
      withModule action = withTempDirectory \ dir -> do
        let
          file :: FilePath
          file = dir </> "Foo.hs"
        writeFile file "module Foo where"
        action file

      failingModule :: String -> IO ()
      failingModule file = writeFile file $ unlines [
          "module Foo where"
        , "foo = bar"
        ]

    it "indicates success" do
      withModule \ file -> do
        withInterpreter [file] \ ghci -> do
          Interpreter.reload ghci `shouldReturn` ("", (Ok, []))

    it "indicates failure" do
      withModule \ file -> do
        withInterpreter [file] \ ghci -> do
          failingModule file
          diagnostics <- ifGhc GHC_910 >>= \ case
            True -> do
              diagnostic <- diagnosticForGhc
              return [
                  Annotated diagnostic {
                    span = Just $ Span file (Location 2 7) (Location 2 10)
                  , code = Just 88464
                  , message = ["Variable not in scope: bar"]
                  } (Just $ NotInScope "bar") []
                ]
            False -> do
              return []
          snd <$> Interpreter.reload ghci `shouldReturn` (Failed, diagnostics)

    context "with -fno-diagnostics-as-json" $ do
      it "does not extract diagnostics" do
        require GHC_910
        withModule \ file -> do
          withInterpreter ["-fno-diagnostics-as-json", file] \ ghci -> do
            failingModule file
            snd <$> Interpreter.reload ghci `shouldReturn` (Failed, [])
