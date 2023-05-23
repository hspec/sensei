{-# LANGUAGE CPP #-}
module Session (
  Config(..)
, Session(..)
, echo
, withSession
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
, hasHspecCommandSignature
, hspecCommand
, parseSummary
#endif
) where

import           Imports

import           Data.IORef
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           Language.Haskell.GhciWrapper hiding (echo)
import qualified Language.Haskell.GhciWrapper as Interpreter

import           Util
import           Options

data Session = Session {
  sessionInterpreter :: Interpreter
, sessionHspecArgs :: [String]
, sessionHspecPreviousSummary :: IORef (Maybe Summary)
}

echo :: Session -> String -> IO ()
echo session = Interpreter.echo (sessionInterpreter session) . encodeUtf8 . T.pack

resetSummary :: Session -> IO ()
resetSummary Session{..} = writeIORef sessionHspecPreviousSummary (Just $ Summary 0 0)

hspecPreviousSummary :: Session -> IO (Maybe Summary)
hspecPreviousSummary Session{..} = readIORef sessionHspecPreviousSummary

withSession :: Config -> [String] -> (Session -> IO r) -> IO r
withSession config args action = do
  withInterpreter config ghciArgs $ \ ghci -> do
    ref <- newIORef (Just $ Summary 0 0)
    action (Session ghci hspecArgs ref)
  where
    (ghciArgs, hspecArgs) = splitArgs args

reload :: Session -> IO String
reload Session{..} = evalVerbose sessionInterpreter ":reload"

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
hasSpec command Session{..} = hasHspecCommandSignature command <$> eval sessionInterpreter (":type " ++ command)

hasHspecCommandSignature :: String -> String -> Bool
hasHspecCommandSignature command = any match . lines . normalizeTypeSignatures
  where
    match line = (command ++ " :: IO ") `isPrefixOf` line && "Summary" `isSuffixOf` line

runSpec :: String -> Session -> IO String
runSpec command session@Session{..} = do
  failedPreviously <- isFailure <$> hspecPreviousSummary session
  let args = "--color" : (if failedPreviously then addRerun else id) sessionHspecArgs
  r <- evalVerbose sessionInterpreter $ "System.Environment.withArgs " ++ show args ++ " $ " ++ command
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
