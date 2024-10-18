{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
module Language.Haskell.GhciWrapper (
  Config(..)
, Interpreter(echo)
, withInterpreter
, eval

, Extract(..)
, partialMessageStartsWithOneOf
, evalVerbose

, ReloadStatus(..)
, reload

#ifdef TEST
, extractReloadStatus
, extractNothing
#endif
) where

import           Imports

import qualified Data.ByteString as ByteString
import           System.IO hiding (stdin, stdout, stderr)
import           System.IO.Temp (withSystemTempFile)
import           System.Environment (getEnvironment)
import           System.Process hiding (createPipe)
import           System.Exit (exitFailure)

import           Util (isWritableByOthers)
import qualified ReadHandle
import           ReadHandle (ReadHandle, toReadHandle, Extract(..), partialMessageStartsWithOneOf)

data Config = Config {
  configIgnoreDotGhci :: Bool
, configWorkingDirectory :: Maybe FilePath
, configEcho :: ByteString -> IO ()
}

data Interpreter = Interpreter {
  hIn  :: Handle
, hOut :: Handle
, readHandle :: ReadHandle
, process :: ProcessHandle
, echo :: ByteString -> IO ()
}

die :: String -> IO a
die = throwIO . ErrorCall

withInterpreter :: Config -> [(String, String)] -> [String] -> (Interpreter -> IO r) -> IO r
withInterpreter config envDefaults args action = do
  withSystemTempFile "sensei" $ \ startupFile h -> do
    hPutStrLn h $ unlines [
        ":set prompt \"\""
      , ":unset +m +r +s +t +c"
      , ":seti -XHaskell2010"
      , ":seti -XNoOverloadedStrings"

      -- GHCi uses NoBuffering for stdout and stderr by default:
      -- https://downloads.haskell.org/ghc/9.4.4/docs/users_guide/ghci.html
      , "GHC.IO.Handle.hSetBuffering System.IO.stdout GHC.IO.Handle.LineBuffering"
      , "GHC.IO.Handle.hSetBuffering System.IO.stderr GHC.IO.Handle.LineBuffering"

      , "GHC.IO.Handle.hSetEncoding System.IO.stdout GHC.IO.Encoding.utf8"
      , "GHC.IO.Handle.hSetEncoding System.IO.stderr GHC.IO.Encoding.utf8"
      ]
    hClose h
    bracket (new startupFile config envDefaults args) close action

sanitizeEnv :: [(String, String)] -> [(String, String)]
sanitizeEnv = filter p
  where
    p ("HSPEC_FAILURES", _) = False
    p _ = True

new :: FilePath -> Config -> [(String, String)] -> [String] -> IO Interpreter
new startupFile Config{..} envDefaults args_ = do
  checkDotGhci
  env <- sanitizeEnv <$> getEnvironment

  let
    mandatoryArgs :: [String]
    mandatoryArgs = ["-fshow-loaded-modules", "--interactive"]

    args :: [String]
    args = "-ghci-script" : startupFile : args_ ++ catMaybes [
        if configIgnoreDotGhci then Just "-ignore-dot-ghci" else Nothing
      ] ++ mandatoryArgs

    ghc :: String
    ghc = fromMaybe "ghc" $ lookup "SENSEI_GHC" env

  (stdoutReadEnd, stdoutWriteEnd) <- createPipe

  (Just stdin_, Nothing, Nothing, processHandle ) <- createProcess (proc ghc args) {
    cwd = configWorkingDirectory
  , env = Just $ envDefaults ++ env
  , std_in  = CreatePipe
  , std_out = UseHandle stdoutWriteEnd
  , std_err = UseHandle stdoutWriteEnd
  }

  setMode stdin_
  readHandle <- toReadHandle stdoutReadEnd 1024

  let
    interpreter = Interpreter {
      hIn = stdin_
    , readHandle
    , hOut = stdoutReadEnd
    , process = processHandle
    , echo = configEcho
    }


  _ <- printStartupMessages interpreter
  getProcessExitCode processHandle >>= \ case
    Just _ -> exitFailure
    Nothing -> return interpreter
  where
    checkDotGhci = unless configIgnoreDotGhci $ do
      let dotGhci = fromMaybe "" configWorkingDirectory </> ".ghci"
      isWritableByOthers dotGhci >>= \ case
        False -> pass
        True -> die $ unlines [
            dotGhci <> " is writable by others, you can fix this with:"
          , ""
          , "    chmod go-w " <> dotGhci <> " ."
          , ""
          ]

    setMode h = do
      hSetBinaryMode h False
      hSetBuffering h LineBuffering
      hSetEncoding h utf8

    printStartupMessages :: Interpreter -> IO (String, [ReloadStatus])
    printStartupMessages interpreter = evalVerbose extractReloadStatus interpreter ""

close :: Interpreter -> IO ()
close Interpreter{..} = do
  hClose hIn
  ReadHandle.drain extractNothing readHandle echo
  hClose hOut
  e <- waitForProcess process
  when (e /= ExitSuccess) $ do
    throwIO (userError $ "Language.Haskell.GhciWrapper.close: Interpreter exited with an error (" ++ show e ++ ")")

putExpression :: Interpreter -> String -> IO ()
putExpression Interpreter{hIn = stdin} e = do
  hPutStrLn stdin e
  ByteString.hPut stdin ReadHandle.marker
  hFlush stdin

data ReloadStatus = Ok | Failed
  deriving (Eq, Show)

extractReloadStatus :: Extract ReloadStatus
extractReloadStatus = Extract {
  isPartialMessage = partialMessageStartsWithOneOf [ok, failed]
, parseMessage = \ case
    line | ByteString.isPrefixOf ok line -> Just (Ok, "")
    line | ByteString.isPrefixOf failed line -> Just (Failed, "")
    _ -> Nothing
} where
    ok = "Ok, modules loaded: "
    failed = "Failed, modules loaded: "

extractNothing :: Extract ()
extractNothing = Extract {
  isPartialMessage = const False
, parseMessage = undefined
}

getResult :: Extract a -> Interpreter -> IO (String, [a])
getResult extract Interpreter{..} = first decodeUtf8 <$> ReadHandle.getResult extract readHandle echo

silent :: ByteString -> IO ()
silent _ = pass

eval :: Interpreter -> String -> IO String
eval ghci = fmap fst . evalVerbose extractNothing ghci {echo = silent}

evalVerbose :: Extract a -> Interpreter -> String -> IO (String, [a])
evalVerbose extract ghci expr = putExpression ghci expr >> getResult extract ghci

reload :: Interpreter -> IO (String, ReloadStatus)
reload ghci = evalVerbose extractReloadStatus ghci ":reload" <&> second \ case
  [Ok] -> Ok
  _ -> Failed
