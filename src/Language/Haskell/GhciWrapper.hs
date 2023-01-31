{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.GhciWrapper (
  Config(..)
, Interpreter
, withInterpreter
, eval
, evalEcho
) where

import           Imports

import qualified Data.ByteString as B
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import           System.IO hiding (stdin, stdout, stderr)
import qualified System.IO as System
import           System.Directory (doesFileExist)
import           System.Process
import           System.Exit (ExitCode(..))

import qualified ReadHandle
import           ReadHandle (ReadHandle, toReadHandle)

data Config = Config {
  configIgnoreDotGhci :: Bool
, configVerbose :: Bool
, configStartupFile :: FilePath
} deriving (Eq, Show)

data Interpreter = Interpreter {
  hIn  :: Handle
, hOut :: Handle
, readHandle :: ReadHandle
, process :: ProcessHandle
}

die :: String -> IO a
die = throwIO . ErrorCall

withInterpreter :: Config -> [String] -> (Interpreter -> IO r) -> IO r
withInterpreter config args = bracket (new config args) close

new :: Config -> [String] -> IO Interpreter
new Config{..} args_ = do

  requireFile configStartupFile

  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess (proc "ghci" args) {
    std_in  = CreatePipe
  , std_out = CreatePipe
  , std_err = Inherit
  }

  setMode stdin_
  readHandle <- toReadHandle stdout_ 1024

  let
    interpreter = Interpreter {
      hIn = stdin_
    , readHandle
    , hOut = stdout_
    , process = processHandle
    }

  _ <- printStartupMessages interpreter

  -- The buffering of stdout and stderr is NoBuffering
  evalThrow interpreter "GHC.IO.Handle.hDuplicateTo System.IO.stdout System.IO.stderr"
  evalThrow interpreter "GHC.IO.Handle.hSetBuffering System.IO.stdout GHC.IO.Handle.LineBuffering"
  evalThrow interpreter "GHC.IO.Handle.hSetBuffering System.IO.stderr GHC.IO.Handle.LineBuffering"
  evalThrow interpreter "GHC.IO.Handle.hSetEncoding System.IO.stdout GHC.IO.Encoding.utf8"
  evalThrow interpreter "GHC.IO.Handle.hSetEncoding System.IO.stderr GHC.IO.Encoding.utf8"

  return interpreter
  where
    requireFile name = do
      exists <- doesFileExist name
      unless exists $ do
        die $ "Required file " <> show name <> " does not exist!"

    args = "-ghci-script" : configStartupFile : args_ ++ catMaybes [
        if configIgnoreDotGhci then Just "-ignore-dot-ghci" else Nothing
      ]

    setMode h = do
      hSetBinaryMode h False
      hSetBuffering h LineBuffering
      hSetEncoding h utf8

    printStartupMessages interpreter
      | configVerbose = evalEcho interpreter ""
      | otherwise = eval interpreter ""

    evalThrow :: Interpreter -> String -> IO ()
    evalThrow interpreter expr = do
      output <- eval interpreter expr
      unless (null output) $ do
        close interpreter
        die output

close :: Interpreter -> IO ()
close repl = do
  hClose $ hIn repl

  -- It is crucial not to close `hOut` before calling `waitForProcess`,
  -- otherwise ghci may not cleanly terminate on SIGINT (ctrl-c) and hang
  -- around consuming 100% CPU.  This happens when ghci tries to print
  -- something to stdout in its signal handler (e.g. when it is blocked in
  -- threadDelay it writes "Interrupted." on SIGINT).
  e <- waitForProcess $ process repl
  hClose $ hOut repl

  when (e /= ExitSuccess) $ do
    throwIO (userError $ "Language.Haskell.GhciWrapper.close: Interpreter exited with an error (" ++ show e ++ ")")

putExpression :: Interpreter -> String -> IO ()
putExpression Interpreter{hIn = stdin} e = do
  hPutStrLn stdin e
  hPutStrLn stdin (ReadHandle.markerString ++ " :: Data.String.String")
  hFlush stdin

getResult :: Interpreter -> (ByteString -> IO ()) -> IO String
getResult Interpreter{readHandle = h} = fmap (T.unpack . decodeUtf8) . ReadHandle.getResult h

echo :: ByteString -> IO ()
echo string = B.putStr string >> hFlush System.stdout

-- | Evaluate an expression
eval :: Interpreter -> String -> IO String
eval repl expr = do
  putExpression repl expr
  getResult repl (const pass)

-- | Evaluate an expression
evalEcho :: Interpreter -> String -> IO String
evalEcho repl expr = do
  putExpression repl expr
  getResult repl echo
