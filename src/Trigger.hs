{-# LANGUAGE CPP #-}
module Trigger (
  trigger
, triggerAll
#ifdef TEST
, reloadedSuccessfully
#endif
) where

import           Imports


import           Util
import           Session (Session, echo, isFailure, isSuccess, hspecPreviousSummary, resetSummary)
import qualified Session

triggerAll :: Session -> IO (Bool, String)
triggerAll session = do
  resetSummary session
  trigger session

reloadedSuccessfully :: String -> Bool
reloadedSuccessfully = any success . lines
  where
    success :: String -> Bool
    success x = case stripPrefix "Ok, " x of
      Just "one module loaded." -> True
      Just "1 module loaded." -> True
      Just xs | [_number, "modules", "loaded."] <- words xs -> True
      Just xs -> "modules loaded: " `isPrefixOf` xs
      Nothing -> False

trigger :: Session -> IO (Bool, String)
trigger session = do
  xs <- Session.reload session
  fmap (xs ++) <$> if reloadedSuccessfully xs
    then do
      echo session $ withColor Green "RELOADING SUCCEEDED"
      hspec
    else do
      echo session $ withColor Red "RELOADING FAILED"
      return (False, "")
  where
    hspec :: IO (Bool, String)
    hspec = do
      mRun <- Session.getRunSpec session
      case mRun of
        Just run -> runSpecs run
        Nothing -> return (True, "")

    runSpecs :: IO String -> IO (Bool, String)
    runSpecs run = do
      failedPreviously <- isFailure <$> hspecPreviousSummary session
      (success, xs) <- runSpec run
      fmap (xs ++) <$> if success && failedPreviously
        then runSpec run
        else return (success, "")

    runSpec :: IO a -> IO (Bool, a)
    runSpec run = do
      xs <- run
      success <- isSuccess <$> hspecPreviousSummary session
      return (success, xs)
