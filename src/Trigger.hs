module Trigger (trigger) where

import           Prelude ()
import           Prelude.Compat
import           Data.List

import           Interpreter (Session, isFailure, isSuccess, hspecPreviousSummary)
import qualified Interpreter

trigger :: Session -> IO (Bool, String)
trigger session = do
  xs <- Interpreter.reload session
  fmap (xs ++) <$> if "Ok, modules loaded:" `isInfixOf` xs
    then hspec
    else return (False, "")
  where
    hspec = do
      hasSpec <- Interpreter.hasSpec session
      if hasSpec
        then runSpecs
        else return (True, "")

    runSpecs = do
      failedPreviously <- isFailure <$> hspecPreviousSummary session
      (success, xs) <- runSpec
      fmap (xs ++) <$> if success && failedPreviously
        then runSpec
        else return (success, "")

    runSpec = do
      xs <- Interpreter.runSpec session
      success <- isSuccess <$> hspecPreviousSummary session
      return (success, xs)
