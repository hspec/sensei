{-# LANGUAGE RecordWildCards #-}
module Interpreter (
  Session
, new
, close
, reload

, Summary(..)
, hspec
) where

import           Prelude ()
import           Prelude.Compat
import           Text.Read.Compat
import           Data.List.Compat

import qualified Language.Haskell.GhciWrapper as GhciWrapper
import           Language.Haskell.GhciWrapper hiding (new, close)

import           Options

data Session = Session {
  sessionInterpreter :: Interpreter
, sessionHspecArgs :: [String]
}

new :: [String] -> IO Session
new args = do
  let (ghciArgs, hspecArgs) = splitArgs args
  ghci <- GhciWrapper.new defaultConfig{configVerbose = True, configIgnoreDotGhci = False} ghciArgs
  _ <- eval ghci (":set prompt " ++ show "")
  _ <- eval ghci ("import qualified System.Environment")
  _ <- eval ghci ("import qualified Test.Hspec.Runner")
  return (Session ghci hspecArgs)

close :: Session -> IO ()
close = GhciWrapper.close . sessionInterpreter

reload :: Session -> IO String
reload Session{..} = evalEcho sessionInterpreter ":reload"

data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show, Read)

hspec :: Session -> IO (String, Maybe Summary)
hspec Session{..} = do
  r <- evalEcho sessionInterpreter $ "System.Environment.withArgs " ++ (show $ "--color" : sessionHspecArgs) ++ " $ Test.Hspec.Runner.hspecResult spec"
  return $ case reverse $ lines r of
    x : _ | Just summary <- readMaybe (dropAnsiEscapeSequences x) -> (r, Just summary)
    _ -> (r, Nothing)
  where
    dropAnsiEscapeSequences xs
      | "Summary" `isPrefixOf` xs = xs
      | otherwise = case xs of
          _ : ys -> dropAnsiEscapeSequences ys
          [] -> []
