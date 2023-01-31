{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.GhciWrapper (
  Config(..)
, Interpreter
, withInterpreter
, eval
, evalVerbose
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
withInterpreter config args = bracket (new config args) (close $ verbosity config)

new :: Config -> [String] -> IO Interpreter
new config@Config{..} args_ = do

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

    printStartupMessages interpreter = evalWith (verbosity config) interpreter ""

    evalThrow :: Interpreter -> String -> IO ()
    evalThrow interpreter expr = do
      output <- eval interpreter expr
      unless (null output) $ do
        close (verbosity config) interpreter
        die output

close :: (ByteString -> IO ()) -> Interpreter -> IO ()
close echo Interpreter{..} = do
  hClose hIn
  ReadHandle.drain readHandle echo
  hClose hOut
  e <- waitForProcess process
  when (e /= ExitSuccess) $ do
    throwIO (userError $ "Language.Haskell.GhciWrapper.close: Interpreter exited with an error (" ++ show e ++ ")")

putExpression :: Interpreter -> String -> IO ()
putExpression Interpreter{hIn = stdin} e = do
  hPutStrLn stdin e
  hPutStrLn stdin (ReadHandle.markerString ++ " :: Data.String.String")
  hFlush stdin

getResult :: Interpreter -> (ByteString -> IO ()) -> IO String
getResult Interpreter{readHandle = h} = fmap (T.unpack . decodeUtf8) . ReadHandle.getResult h

verbosity :: Config -> ByteString -> IO ()
verbosity config
  | configVerbose config = verbose
  | otherwise = silent

verbose :: ByteString -> IO ()
verbose string = B.putStr string >> hFlush System.stdout

silent :: ByteString -> IO ()
silent _ = pass

evalWith :: (ByteString -> IO ()) -> Interpreter -> String -> IO String
evalWith echo repl expr = do
  putExpression repl expr
  getResult repl echo

eval :: Interpreter -> String -> IO String
eval = evalWith silent

evalVerbose :: Interpreter -> String -> IO String
evalVerbose = evalWith verbose
