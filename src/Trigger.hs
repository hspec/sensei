module Trigger (trigger) where

import           Prelude ()
import           Prelude.Compat
import           Data.List

import           Interpreter (Session, Summary(..))
import qualified Interpreter

trigger :: Session -> IO (Bool, String)
trigger interpreter = do
  xs <- Interpreter.reload interpreter
  (ys, summary) <- if "Ok, modules loaded:" `isInfixOf` xs
    then Interpreter.hspec interpreter
    else return ("", Nothing)
  return (maybe False ((== 0) . summaryFailures) summary, xs ++ ys)
