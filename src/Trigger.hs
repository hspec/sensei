module Trigger (
  trigger
, triggerAll
) where

import           Prelude ()
import           Prelude.Compat
import           Data.List

import           Session (Session, isFailure, isSuccess, hspecPreviousSummary, resetSummary)
import qualified Session

triggerAll :: Session -> IO (Bool, String)
triggerAll session = do
  resetSummary session
  trigger session

trigger :: Session -> IO (Bool, String)
trigger session = do
  xs <- Session.reload session
  fmap (xs ++) <$> if "Ok, modules loaded:" `isInfixOf` xs
    then hspec
    else return (False, "")
  where
    hspec = do
      hasSpec <- Session.hasSpec session
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
      xs <- Session.runSpec session
      success <- isSuccess <$> hspecPreviousSummary session
      return (success, xs)
