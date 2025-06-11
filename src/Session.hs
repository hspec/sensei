{-# LANGUAGE CPP #-}
module Session (
  Config(..)
, Session(..)
, echo
, withSession

, ReloadStatus(..)
, reload

, modules

, Summary(..)
, resetSummary
, hspecPreviousSummary
, isFailure
, isSuccess
, getRunSpec

#ifdef TEST
, runSpec
, hasSpec
, hasHspecCommandSignature
, hspecCommand
, Extract(..)
, extractSummary
, ansiShowCursor
#endif
) where

import           Imports

import qualified Data.ByteString as ByteString

import           Language.Haskell.GhciWrapper hiding (reload)
import qualified Language.Haskell.GhciWrapper as Interpreter

import           Util
import           Options
import           GHC.Diagnostic

data Session = Session {
  interpreter :: Interpreter
, getAvailableImports :: IO AvailableImports
, hspecArgs :: [String]
, hspecPreviousSummaryRef :: IORef (Maybe Summary)
}

echo :: Session -> String -> IO ()
echo session = session.interpreter.echo . encodeUtf8

resetSummary :: Session -> IO ()
resetSummary session = writeIORef session.hspecPreviousSummaryRef (Just $ Summary 0 0)

hspecPreviousSummary :: MonadIO m => Session -> m (Maybe Summary)
hspecPreviousSummary session = liftIO $ readIORef session.hspecPreviousSummaryRef

envDefaults :: [(String, String)]
envDefaults = [("HSPEC_EXPERT", "yes")]

withSession :: IO AvailableImports -> Config -> [String] -> (Session -> IO r) -> IO r
withSession getAvailableImports config args action = do
  withInterpreter config envDefaults ("-Werror" : ghciArgs) $ \ ghci -> do
    ref <- newIORef (Just $ Summary 0 0)
    action (Session ghci getAvailableImports hspecArgs ref)
  where
    (ghciArgs, hspecArgs) = splitArgs args

reload :: MonadIO m => Session -> m (String, (ReloadStatus, [Annotated]))
reload session = liftIO $ Interpreter.reload session.getAvailableImports session.interpreter

modules :: Session -> IO [String]
modules session = map read . drop 1 . lines <$> eval session.interpreter ":complete repl \"import \""

data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show, Read)

hspecCommand :: String
hspecCommand = "Test.Hspec.Runner.hspecResult spec"

hspecCoreCommand :: String
hspecCoreCommand = "Test.Hspec.Core.Runner.hspecResult spec"

hspecMetaCommand :: String
hspecMetaCommand = "Test.Hspec.Meta.hspecResult spec"

getRunSpec :: Session -> IO (Maybe (IO String))
getRunSpec session = go [hspecCommand, hspecCoreCommand, hspecMetaCommand]
  where
    go = \ case
      [] -> return Nothing
      command : commands -> do
        getRunSpecWith command session >>= \ case
          r@Just{} -> return r
          Nothing -> go commands

getRunSpecWith :: String -> Session -> IO (Maybe (IO String))
getRunSpecWith command session = do
  has <- hasSpec command session
  if has
    then return $ Just (runSpec command session)
    else return Nothing

hasSpec :: String -> Session -> IO Bool
hasSpec command session = hasHspecCommandSignature command <$> eval session.interpreter (":type " ++ command)

hasHspecCommandSignature :: String -> String -> Bool
hasHspecCommandSignature command = any match . lines . normalizeTypeSignatures
  where
    match line = (command ++ " :: IO ") `isPrefixOf` line && "Summary" `isSuffixOf` line

runSpec :: String -> Session -> IO String
runSpec command session = do
  failedPreviously <- isFailure <$> hspecPreviousSummary session
  let args = "--color" : (if failedPreviously then addRerun else id) session.hspecArgs
  (r, summary) <- evalVerbose extractSummary session.interpreter $ "System.Environment.withArgs " ++ show args ++ " $ " ++ command
  writeIORef session.hspecPreviousSummaryRef . listToMaybe $ reverse summary
  return r
  where
    addRerun :: [String] -> [String]
    addRerun args = "--rerun" : args

isFailure :: Maybe Summary -> Bool
isFailure = maybe True ((/= 0) . (.summaryFailures))

isSuccess :: Maybe Summary -> Bool
isSuccess = not . isFailure

extractSummary :: Extract Summary
extractSummary = Extract {
  isPartialMessage = partialMessageStartsWithOneOf [summaryPrefix, ansiShowCursor <> summaryPrefix]
, parseMessage
} where
    summaryPrefix :: ByteString
    summaryPrefix = "Summary {"

    parseMessage :: ByteString -> IO (Maybe (Summary, ByteString))
    parseMessage input = return case ByteString.stripPrefix ansiShowCursor input of
      Nothing -> flip (,) "" <$> parseSummary input
      Just i -> flip (,) ansiShowCursor <$> parseSummary i

    parseSummary :: ByteString -> Maybe Summary
    parseSummary = readMaybe . decodeUtf8 . stripAnsiShowCursor

    stripAnsiShowCursor :: ByteString -> ByteString
    stripAnsiShowCursor input = fromMaybe input $ ByteString.stripPrefix ansiShowCursor input

ansiShowCursor :: ByteString
ansiShowCursor = "\ESC[?25h"
