module Interpreter (
  new
, close
, trigger
) where

import qualified Language.Haskell.GhciWrapper as GhciWrapper
import           Language.Haskell.GhciWrapper hiding (new)

new :: [String] -> IO Interpreter
new args = do
  ghci <- GhciWrapper.new defaultConfig{configVerbose = True, configIgnoreDotGhci = False} args
  _ <- eval ghci (":set prompt " ++ show "")
  return ghci

trigger :: Interpreter -> IO String
trigger ghci = eval ghci ":reload"
