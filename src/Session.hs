{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Session (
  Session(..)
, new
, close
, reload

, Summary(..)
, resetSummary
, hspecPreviousSummary
, isFailure
, isSuccess
, getRunSpec

#ifdef TEST
, runSpec
, hasSpec
, hspecFailureEnvName
, hasHspecCommandSignature
, hspecCommand
, parseSummary
#endif
) where

import           Imports

import           Data.IORef

import qualified Language.Haskell.GhciWrapper as GhciWrapper
import           Language.Haskell.GhciWrapper hiding (new, close)

import           Util
import           Options

hspecFailureEnvName :: String
hspecFailureEnvName = "HSPEC_FAILURES"

data Session = Session {
  sessionInterpreter :: Interpreter
, sessionHspecArgs :: [String]
, sessionHspecPreviousSummary :: IORef (Maybe Summary)
}

resetSummary :: Session -> IO ()
resetSummary Session{..} = writeIORef sessionHspecPreviousSummary (Just $ Summary 0 0)

hspecPreviousSummary :: Session -> IO (Maybe Summary)
hspecPreviousSummary Session{..} = readIORef sessionHspecPreviousSummary

new :: [String] -> IO Session
new args = do
  let (ghciArgs, hspecArgs) = splitArgs args
  ghci <- GhciWrapper.new defaultConfig{configVerbose = True, configIgnoreDotGhci = False} ghciArgs
  _ <- eval ghci (":set prompt " ++ show "")
  _ <- eval ghci ("System.Environment.unsetEnv " ++ show hspecFailureEnvName)
  ref <- newIORef (Just $ Summary 0 0)
  return (Session ghci hspecArgs ref)

close :: Session -> IO ()
close = GhciWrapper.close . sessionInterpreter

reload :: Session -> IO String
reload Session{..} = evalEcho sessionInterpreter ":reload"

data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show, Read)

hspecCommand :: String
hspecCommand = "Test.Hspec.Runner.hspecResult spec"

hspecMetaCommand :: String
hspecMetaCommand = "Test.Hspec.Meta.hspecResult spec"

getRunSpec :: Session -> IO (Maybe (IO String))
getRunSpec session = do
  r <- getRunSpecWith hspecCommand session
  case r of
    Just _  -> return r
    Nothing -> getRunSpecWith hspecMetaCommand session

getRunSpecWith :: String -> Session -> IO (Maybe (IO String))
getRunSpecWith command session = do
  has <- hasSpec command session
  if has
    then return $ Just (runSpec command session)
    else return Nothing

hasSpec :: String -> Session -> IO Bool
hasSpec command Session{..} = hasHspecCommandSignature command <$> eval sessionInterpreter (":type " ++ command)

hasHspecCommandSignature :: String -> String -> Bool
hasHspecCommandSignature command = any match . lines . normalizeTypeSignatures
  where
    match line = (command ++ " :: IO ") `isPrefixOf` line && "Summary" `isSuffixOf` line

runSpec :: String -> Session -> IO String
runSpec command session@Session{..} = do
  failedPreviously <- isFailure <$> hspecPreviousSummary session
  let args = "--color" : (if failedPreviously then addRerun else id) sessionHspecArgs
  r <- evalEcho sessionInterpreter $ "System.Environment.withArgs " ++ show args ++ " $ " ++ command
  writeIORef sessionHspecPreviousSummary (parseSummary r)
  return r
  where
    addRerun :: [String] -> [String]
    addRerun args = "--rerun" : args

isFailure :: Maybe Summary -> Bool
isFailure = maybe True ((/= 0) . summaryFailures)

isSuccess :: Maybe Summary -> Bool
isSuccess = not . isFailure

parseSummary :: String -> Maybe Summary
parseSummary = findJust . map (readMaybe . dropAnsiEscapeSequences) . reverse . lines
  where
    findJust = listToMaybe . catMaybes

    dropAnsiEscapeSequences xs
      | "Summary" `isPrefixOf` xs = xs
      | otherwise = case xs of
          _ : ys -> dropAnsiEscapeSequences ys
          [] -> []
