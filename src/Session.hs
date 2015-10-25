{-# LANGUAGE RecordWildCards #-}
module Session (
  Session(..)
, hspecFailureEnvName
, new
, close
, reload

, Summary(..)
, isFailure
, isSuccess
, hasSpec
, hasSpecString
, runSpec
, hspecPreviousSummary
, resetSummary
, parseSummary
) where

import           Data.IORef
import           Data.List.Compat
import           Data.Maybe (listToMaybe, catMaybes)
import           Prelude ()
import           Prelude.Compat
import           Text.Read.Compat

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
  _ <- eval ghci ("import qualified System.Environment")
  _ <- eval ghci ("import qualified Test.Hspec.Runner")
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

hasSpec :: Session -> IO Bool
hasSpec Session{..} =
  hasSpecString . normalizeTypeSignatures <$> eval sessionInterpreter (":type " ++ hspecCommand)

hasSpecString :: String -> Bool
hasSpecString = any match . lines
  where
    match line = (hspecCommand ++ " :: IO ") `isPrefixOf` line
              && "Summary" `isSuffixOf` line

runSpec :: Session -> IO String
runSpec session@Session{..} = do
  failedPreviously <- isFailure <$> hspecPreviousSummary session
  let args = "--color" : (if failedPreviously then addRerun else id) sessionHspecArgs
  r <- evalEcho sessionInterpreter $ "System.Environment.withArgs " ++ show args ++ " $ " ++ hspecCommand
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
parseSummary = findJust . (map $ readMaybe . dropAnsiEscapeSequences) . reverse . lines
  where
    findJust = listToMaybe . catMaybes

    dropAnsiEscapeSequences xs
      | "Summary" `isPrefixOf` xs = xs
      | otherwise = case xs of
          _ : ys -> dropAnsiEscapeSequences ys
          [] -> []
