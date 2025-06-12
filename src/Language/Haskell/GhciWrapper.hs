{-# LANGUAGE CPP #-}
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
, sensei_ghc
, sensei_ghc_version
, lookupGhc
, lookupGhcVersion
, numericVersion
, extractReloadDiagnostics
, extractDiagnostics
#endif
) where

import           Imports

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as T
import           System.IO hiding (stdin, stdout, stderr)
import           System.IO.Temp (withSystemTempFile)
import           System.Environment (getEnvironment)
import           System.Process hiding (createPipe)
import           System.Exit (exitFailure)

import           Util (isWritableByOthers)
import           ReadHandle hiding (getResult)
import qualified ReadHandle
import           GHC.Diagnostic (Annotated)
import qualified GHC.Diagnostic as Diagnostic

sensei_ghc_version :: String
sensei_ghc_version = "SENSEI_GHC_VERSION"

sensei_ghc :: String
sensei_ghc = "SENSEI_GHC"

lookupGhc :: [(String, String)] -> FilePath
lookupGhc = fromMaybe "ghc" . lookup sensei_ghc

lookupGhcVersion :: [(String, String)] -> Maybe String
lookupGhcVersion = lookup sensei_ghc_version

getGhcVersion :: FilePath -> [(String, String)] -> IO String
getGhcVersion ghc env = case lookupGhcVersion env of
  Nothing -> numericVersion ghc
  Just version -> return version

numericVersion :: [Char] -> IO [Char]
numericVersion ghc = strip <$> readProcess ghc ["--numeric-version"] ""

data Config = Config {
  configIgnoreDotGhci :: Bool
, configIgnore_GHC_ENVIRONMENT :: Bool
, configWorkingDirectory :: Maybe FilePath
, configHieDirectory :: Maybe FilePath
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

sanitizeEnv :: Config -> [(String, String)] -> [(String, String)]
sanitizeEnv config = filter p
  where
    p ("HSPEC_FAILURES", _) = False
    p ("GHC_ENVIRONMENT", _) = not config.configIgnore_GHC_ENVIRONMENT
    p _ = True

new :: FilePath -> Config -> [(String, String)] -> [String] -> IO Interpreter
new startupFile config@Config{..} envDefaults args_ = do
  checkDotGhci
  env <- sanitizeEnv config <$> getEnvironment

  let
    ghc :: String
    ghc = lookupGhc env

  ghcVersion <- parseVersion <$> getGhcVersion ghc env

  let
    diagnosticsAsJson :: [String] -> [String]
    diagnosticsAsJson
      | ghcVersion < Just (makeVersion [9,10]) = id
      | otherwise = ("-fdiagnostics-as-json" :)

    writeIdeInfo :: [String]
    writeIdeInfo = case configHieDirectory of
      Just dir | ghcVersion >= Just (makeVersion [8,8]) -> ["-fwrite-ide-info", "-hiedir", dir]
      _ -> []

    mandatoryArgs :: [String]
    mandatoryArgs = ["-fshow-loaded-modules", "--interactive"]

    args :: [String]
    args = "-ghci-script" : startupFile : diagnosticsAsJson args_ ++ catMaybes [
        if configIgnoreDotGhci then Just "-ignore-dot-ghci" else Nothing
      ] ++ writeIdeInfo ++ mandatoryArgs

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
    checkDotGhci :: IO ()
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

    setMode :: Handle -> IO ()
    setMode h = do
      hSetBinaryMode h False
      hSetBuffering h LineBuffering
      hSetEncoding h utf8

    printStartupMessages :: Interpreter -> IO (String, [Either ReloadStatus Annotated])
    printStartupMessages ghci = evalVerbose (extractReloadDiagnostics mempty) ghci ""

close :: Interpreter -> IO ()
close ghci = do
  hClose ghci.hIn
  ReadHandle.drain (extractReloadDiagnostics mempty) ghci.readHandle ghci.echo
  hClose ghci.hOut
  e <- waitForProcess ghci.process
  when (e /= ExitSuccess) $ do
    throwIO (userError $ "Language.Haskell.GhciWrapper.close: Interpreter exited with an error (" ++ show e ++ ")")

putExpression :: Interpreter -> String -> IO ()
putExpression Interpreter{hIn = stdin} e = do
  hPutStrLn stdin e
  ByteString.hPut stdin ReadHandle.marker
  hFlush stdin

extractReloadDiagnostics :: IO Diagnostic.AvailableImports -> Extract (Either ReloadStatus Annotated)
extractReloadDiagnostics getAvailableImports = extractReloadStatus <+> extractAnnotatedDiagnostics getAvailableImports

data ReloadStatus = Ok | Failed
  deriving (Eq, Show)

extractReloadStatus :: Extract ReloadStatus
extractReloadStatus = Extract {
  isPartialMessage = partialMessageStartsWithOneOf [ok, failed]
, parseMessage = \ case
    line | ByteString.isPrefixOf ok line -> accept (Ok, "")
    line | ByteString.isPrefixOf failed line -> accept (Failed, "")
    _ -> reject
} where
    accept = return . Just
    reject = return Nothing
    ok = "Ok, modules loaded: "
    failed = "Failed, modules loaded: "

extractAnnotatedDiagnostics :: IO Diagnostic.AvailableImports -> ReadHandle.Extract Annotated
extractAnnotatedDiagnostics getAvailableImports = ReadHandle.Extract {
  isPartialMessage = ByteString.isPrefixOf "{"
, parseMessage = \ input -> fmap (id &&& T.encodeUtf8 . Diagnostic.formatAnnotated) <$> Diagnostic.parseAnnotated getAvailableImports input
}

extractDiagnostics :: ReadHandle.Extract Diagnostic.Diagnostic
extractDiagnostics = ReadHandle.Extract {
  isPartialMessage = ByteString.isPrefixOf "{"
, parseMessage = \ input -> return $ (id &&& encodeUtf8 . Diagnostic.format) <$> Diagnostic.parse input
}

getResult :: Extract a -> Interpreter -> IO (String, [a])
getResult extract Interpreter{..} = first decodeUtf8 <$> ReadHandle.getResult extract readHandle echo

silent :: ByteString -> IO ()
silent _ = pass

eval :: Interpreter -> String -> IO String
eval ghci = fmap fst . evalVerbose extractDiagnostics ghci {echo = silent}

evalVerbose :: Extract a -> Interpreter -> String -> IO (String, [a])
evalVerbose extract ghci expr = putExpression ghci expr >> getResult extract ghci

reload :: IO Diagnostic.AvailableImports -> Interpreter -> IO (String, (ReloadStatus, [Annotated]))
reload getAvailableImports ghci = do
  evalVerbose (extractReloadDiagnostics getAvailableImports) ghci ":reload" <&> second \ case
    (partitionEithers -> ([Ok], diagnostics)) -> (Ok, diagnostics)
    (partitionEithers ->(_, diagnostics)) -> (Failed, diagnostics)
