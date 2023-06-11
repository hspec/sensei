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

#if MIN_VERSION_mtl(2,3,1)
import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS hiding (pass)
#else
import           Control.Monad.Writer.Strict hiding (pass)
#endif

import           Control.Monad.Except

import           Util
import           Session (Session, isFailure, isSuccess, hspecPreviousSummary, resetSummary)
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

type Trigger = ExceptT () (WriterT String IO)

trigger :: Session -> IO (Result, String)
trigger session = runWriterT (runExceptT go) >>= \ case
  (Left (), output) -> return (Failure, output)
  (Right (), output) -> return (Success, output)
  where
    go :: Trigger ()
    go = do
      output <- Session.reload session
      tell output
      case reloadedSuccessfully output of
        False -> do
          echo $ withColor Red "RELOADING FAILED" <> "\n"
          abort
        True -> do
          echo $ withColor Green "RELOADING SUCCEEDED" <> "\n"

      getRunSpec >>= \ case
        Just hspec -> rerunAllOnSuccess hspec
        Nothing -> pass

    abort :: Trigger a
    abort = throwError ()

    rerunAllOnSuccess :: Trigger () -> Trigger ()
    rerunAllOnSuccess hspec = do
      failedPreviously <- isFailure <$> hspecPreviousSummary session
      hspec
      when failedPreviously hspec

    getRunSpec :: MonadIO m => m (Maybe (Trigger ()))
    getRunSpec = liftIO $ fmap runSpec <$> Session.getRunSpec session

    runSpec :: IO String -> Trigger ()
    runSpec hspec = do
      liftIO hspec >>= tell . removeProgress
      result <- hspecPreviousSummary session
      unless (isSuccess result) abort

    echo :: String -> Trigger ()
    echo message = do
      tell message
      liftIO $ Session.echo session message
