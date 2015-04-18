{-# LANGUAGE RecordWildCards #-}
module Interpreter (
  Session
, new
, close
, reload
, hspec
) where

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
  ghci <- GhciWrapper.new defaultConfig{configVerbose = True, configIgnoreDotGhci = False, configGhci = "cabal"} ("exec" : "ghci" : "--" : ghciArgs)
  _ <- eval ghci (":set prompt " ++ show "")
  _ <- eval ghci ("import qualified System.Environment")
  _ <- eval ghci ("import qualified Test.Hspec")
  return (Session ghci hspecArgs)

close :: Session -> IO ()
close = GhciWrapper.close . sessionInterpreter

reload :: Session -> IO String
reload Session{..} = evalEcho sessionInterpreter ":reload"

hspec :: Session -> IO String
hspec Session{..} = evalEcho sessionInterpreter $ "System.Environment.withArgs " ++ (show $ "--color" : sessionHspecArgs) ++ " $ Test.Hspec.hspec spec"
