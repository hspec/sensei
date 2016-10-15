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
      mRun <- Session.getRunSpec session
      case mRun of
        Just run -> runSpecs run
        Nothing -> return (True, "")

    runSpecs run = do
      failedPreviously <- isFailure <$> hspecPreviousSummary session
      (success, xs) <- runSpec run
      fmap (xs ++) <$> if success && failedPreviously
        then runSpec run
        else return (success, "")

    runSpec run = do
      xs <- run
      success <- isSuccess <$> hspecPreviousSummary session
      return (success, xs)
