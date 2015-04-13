module Interpreter (
  Interpreter
, new
, close
, reload
, hspec
) where

import qualified Language.Haskell.GhciWrapper as GhciWrapper
import           Language.Haskell.GhciWrapper hiding (new)

new :: [String] -> IO Interpreter
new args = do
  ghci <- GhciWrapper.new defaultConfig{configVerbose = True, configIgnoreDotGhci = False} args
  _ <- eval ghci (":set prompt " ++ show "")
  return ghci

reload :: Interpreter -> IO String
reload ghci = evalEcho ghci ":reload"

hspec :: Interpreter -> IO String
hspec ghci = evalEcho ghci ":main --color"
