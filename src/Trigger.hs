{-# LANGUAGE CPP #-}
module Trigger (
  Result(..)
, trigger
, triggerAll
#ifdef TEST
, reloadedSuccessfully
, removeProgress
#endif
) where

import           Imports

import           Util
import           Session (Session, echo, isFailure, isSuccess, hspecPreviousSummary, resetSummary)
import qualified Session

data Result = Failure | Success
  deriving (Eq, Show)

triggerAll :: Session -> IO (Result, String)
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

removeProgress :: String -> String
removeProgress xs = case break (== '\r') xs of
  (_, "") -> xs
  (ys, _ : zs) -> dropLastLine ys ++ removeProgress zs
  where
    dropLastLine :: String -> String
    dropLastLine = reverse . dropWhile (/= '\n') . reverse

trigger :: Session -> IO (Result, String)
trigger session = do
  output <- Session.reload session

  let
    result
      | reloadedSuccessfully output = Success
      | otherwise = Failure

    message = case result of
      Failure -> withColor Red "RELOADING FAILED" <> "\n"
      Success -> withColor Green "RELOADING SUCCEEDED" <> "\n"

  echo session message

  fmap removeProgress . fmap (output <>) . fmap (message <>) <$> case result of
    Failure -> return (result, "")
    Success -> hspec
  where
    hspec :: IO (Result, String)
    hspec = do
      mRun <- Session.getRunSpec session
      case mRun of
        Just run -> rerunAllOnSuccess run
        Nothing -> return (Success, "")

    rerunAllOnSuccess :: IO String -> IO (Result, String)
    rerunAllOnSuccess run = do
      failedPreviously <- isFailure <$> hspecPreviousSummary session
      (result, xs) <- runSpec run
      fmap (xs ++) <$> if result == Success && failedPreviously
        then runSpec run
        else return (result, "")

    runSpec :: IO a -> IO (Result, a)
    runSpec run = do
      xs <- run
      result <- isSuccess <$> hspecPreviousSummary session >>= \ case
        False -> return Failure
        True -> return Success
      return (result, xs)
