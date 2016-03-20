module Trigger (
  trigger
, triggerAll
) where

import           Data.List
import           Prelude        ()
import           Prelude.Compat

import           Session        (Session, hspecPreviousSummary, isFailure,
                                 isSuccess, resetSummary, sessionTestFlag)
import qualified Session

triggerAll :: Session -> IO (Bool, String)
triggerAll session = do
  resetSummary session
  trigger session

trigger :: Session -> IO (Bool, String)
trigger session = do
  xs <- Session.reload session
  let compilationSuccess = "Ok, modules loaded:" `isInfixOf` xs
  fmap (xs ++) <$> runIf (sessionTestFlag session) compilationSuccess
    where
      runIf True  True  = hspec              -- ^ run tests
      runIf False True  = return (True, "")  -- ^ only compile
      runIf _     _     = return (False, "") -- ^ compilation failed

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

