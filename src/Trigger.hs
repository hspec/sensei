{-# LANGUAGE CPP #-}
module Trigger (
  Hook
, HookResult(..)
, Hooks(..)
, Result(..)
, trigger
, triggerAll
#ifdef TEST
, removeProgress
#endif
) where

import           Imports

import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS hiding (pass)

import           Control.Monad.Except

import           Util
import           Config (Hook, HookResult(..))
import           Session (Session, ReloadStatus(..), isFailure, isSuccess, hspecPreviousSummary, resetSummary)
import qualified Session
import           GHC.Diagnostic

data Hooks = Hooks {
  beforeReload :: Hook
, afterReload :: Hook
}

data Result = HookFailed | Failure | Success
  deriving (Eq, Show)

triggerAll :: Session -> Hooks -> IO (Result, String, [Diagnostic])
triggerAll session hooks = do
  resetSummary session
  trigger session hooks

removeProgress :: String -> String
removeProgress xs = case break (== '\r') xs of
  (_, "") -> xs
  (ys, _ : zs) -> dropLastLine ys ++ removeProgress zs
  where
    dropLastLine :: String -> String
    dropLastLine = reverse . dropWhile (/= '\n') . reverse

type Trigger = ExceptT Result (WriterT (String, [Diagnostic]) IO)

trigger :: Session -> Hooks -> IO (Result, String, [Diagnostic])
trigger session hooks = runWriterT (runExceptT go) >>= \ case
  (Left result, (output, diagnostics)) -> return (result, output, diagnostics)
  (Right (), (output, diagnostics)) -> return (Success, output, diagnostics)
  where
    go :: Trigger ()
    go = do
      runHook hooks.beforeReload
      (output, (r, diagnostics)) <- Session.reload session
      tell (output, diagnostics)
      case r of
        Failed -> do
          echo $ withColor Red "RELOADING FAILED" <> "\n"
          abort
        Ok -> do
          echo $ withColor Green "RELOADING SUCCEEDED" <> "\n"

      runHook hooks.afterReload
      getRunSpec >>= \ case
        Just hspec -> rerunAllOnSuccess hspec
        Nothing -> pass

    abort :: Trigger a
    abort = throwError Failure

    rerunAllOnSuccess :: Trigger () -> Trigger ()
    rerunAllOnSuccess hspec = do
      failedPreviously <- isFailure <$> hspecPreviousSummary session
      hspec
      when failedPreviously hspec

    getRunSpec :: MonadIO m => m (Maybe (Trigger ()))
    getRunSpec = liftIO $ fmap runSpec <$> Session.getRunSpec session

    runSpec :: IO String -> Trigger ()
    runSpec hspec = do
      r <- removeProgress <$> liftIO hspec
      tell (r, [])
      result <- hspecPreviousSummary session
      unless (isSuccess result) abort

    runHook :: Hook -> Trigger ()
    runHook hook = liftIO hook >>= \ case
      HookSuccess -> pass
      HookFailure message -> echo message >> throwError HookFailed

    echo :: String -> Trigger ()
    echo message = do
      tell (message, [])
      liftIO $ Session.echo session message
