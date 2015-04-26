module Trigger (trigger) where

import           Prelude ()
import           Prelude.Compat
import           Data.List

import           Interpreter (Session, Summary(..))
import qualified Interpreter

trigger :: Session -> IO (Bool, String)
trigger session = do
  xs <- Interpreter.reload session
  fmap (xs ++) <$> if "Ok, modules loaded:" `isInfixOf` xs
    then do
      hasSpec <- Interpreter.hasSpec session
      if hasSpec then do
        (ys, summary) <- Interpreter.runSpec session
        return (maybe False ((== 0) . summaryFailures) summary, ys)
      else
        return (True, "")
    else return (False, "")
