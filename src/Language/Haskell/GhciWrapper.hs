{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.GhciWrapper (
  Interpreter
, Config(..)
, defaultConfig
, new
, close
, eval
, evalEcho
) where

import           Imports
import           System.IO hiding (stdin, stdout, stderr)
import           System.Process
import           System.Exit

data Config = Config {
  configGhci :: String
, configVerbose :: Bool
, configIgnoreDotGhci :: Bool
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  configGhci = "ghci"
, configVerbose = False
, configIgnoreDotGhci = True
}

-- | Truly random marker, used to separate expressions.
--
-- IMPORTANT: This module relies upon the fact that this marker is unique.  It
-- has been obtained from random.org.  Do not expect this module to work
-- properly, if you reuse it for any purpose!
marker :: String
marker = show "dcbd2a1e20ae519a1c7714df2859f1890581d57fac96ba3f499412b2f5c928a1"

data Interpreter = Interpreter {
  hIn  :: Handle
, hOut :: Handle
, process :: ProcessHandle
}

new :: Config -> [String] -> IO Interpreter
new Config{..} args_ = do
  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess (proc configGhci args) {
    std_in  = CreatePipe
  , std_out = CreatePipe
  , std_err = Inherit
  }
  setMode stdin_
  setMode stdout_
  let interpreter = Interpreter {hIn = stdin_, hOut = stdout_, process = processHandle}

  -- The buffering of stdout and stderr is NoBuffering
  evalThrow interpreter "GHC.IO.Handle.hDuplicateTo System.IO.stdout System.IO.stderr"
  evalThrow interpreter "GHC.IO.Handle.hSetBuffering System.IO.stdout GHC.IO.Handle.LineBuffering"
  evalThrow interpreter "GHC.IO.Handle.hSetBuffering System.IO.stderr GHC.IO.Handle.LineBuffering"
  evalThrow interpreter "GHC.IO.Handle.hSetEncoding System.IO.stdout GHC.IO.Encoding.utf8"
  evalThrow interpreter "GHC.IO.Handle.hSetEncoding System.IO.stderr GHC.IO.Encoding.utf8"

  return interpreter
  where
    args = args_ ++ catMaybes [
        if configIgnoreDotGhci then Just "-ignore-dot-ghci" else Nothing
      , if configVerbose then Nothing else Just "-v0"
      ]
    setMode h = do
      hSetBinaryMode h False
      hSetBuffering h LineBuffering
      hSetEncoding h utf8

    evalThrow :: Interpreter -> String -> IO ()
    evalThrow interpreter expr = do
      output <- eval interpreter expr
      unless (null output || configVerbose) $ do
        close interpreter
        throwIO (ErrorCall output)

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
  hPutStrLn stdin (marker ++ " :: Data.String.String")
  hFlush stdin

getResult :: Bool -> Interpreter -> IO String
getResult echoMode Interpreter{hOut = stdout} = go
  where
    go = do
      line <- hGetLine stdout
      if marker `isSuffixOf` line
        then do
          let xs = stripMarker line
          echo xs
          return xs
        else do
          echo (line ++ "\n")
          result <- go
          return (line ++ "\n" ++ result)
    stripMarker l = take (length l - length marker) l

    echo :: String -> IO ()
    echo
      | echoMode = putStr
      | otherwise = \ _ -> return ()

-- | Evaluate an expression
eval :: Interpreter -> String -> IO String
eval repl expr = do
  putExpression repl expr
  getResult False repl

-- | Evaluate an expression
evalEcho :: Interpreter -> String -> IO String
evalEcho repl expr = do
  putExpression repl expr
  getResult True repl
