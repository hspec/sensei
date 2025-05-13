{-# LANGUAGE CPP #-}
module Trigger (
  Hook
, HookResult(..)
, Hooks(..)
, Result(..)
, trigger
, triggerAll

, ModuleIndex(..)
, Modules
#ifdef TEST
, removeProgress
#endif
) where

import Data.Map qualified as Map
import Data.Double.Conversion.Text qualified as Double
import           Prelude ()
import           Imports

import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Control.Monad.Writer.CPS hiding (pass)

import           Control.Monad.Except

import           Util
import           Config (Hook, HookResult(..))
import           Session (Session, ReloadStatus(..), isFailure, isSuccess, hspecPreviousSummary, resetSummary)
import qualified Session
import           GHC.Diagnostic
import           GHC.Diagnostic.Annotated

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.Haskell.GhciWrapper

data ModuleIndex = ModuleIndex {
  globalModules :: Modules
, modules :: IORef Modules
, globalIdentifierMap :: IORef AvailableImports
, globalIdentifierMapConstructionTime :: IORef Double
}

type Modules = Set String

data Hooks = Hooks {
  beforeReload :: Hook
, afterReload :: Hook
}

data Result = HookFailed | Failure | Success
  deriving (Eq, Show)

triggerAll :: ModuleIndex -> Session -> Hooks -> IO (Result, String, [Annotated])
triggerAll moduleIndex session hooks = do
  resetSummary session
  trigger moduleIndex session hooks

removeProgress :: String -> String
removeProgress xs = case break (== '\r') xs of
  (_, "") -> xs
  (ys, _ : zs) -> dropLastLine ys ++ removeProgress zs
  where
    dropLastLine :: String -> String
    dropLastLine = reverse . dropWhile (/= '\n') . reverse

type Trigger = ExceptT Result (WriterT (String, [Annotated]) IO)

trigger :: ModuleIndex -> Session -> Hooks -> IO (Result, String, [Annotated])
trigger moduleIndex session hooks = runWriterT (runExceptT go) >>= \ case
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
          do
            dt <- liftIO $ atomicReadIORef moduleIndex.globalIdentifierMapConstructionTime
            if dt == 0 then do
              echo $ withColor Yellow "constructing global identifier map..." <> "\n"
            else do
              let
                t = Double.toFixed 2 dt
              echo $ "constructing global identifier map done in " <> T.unpack t <> "s"
              echo $ withColor Green " ✔\n"

          echo $ "constructing local identifier map... "
          ((), dt) <- liftIO $ timeAction do
            atomicReadIORef moduleIndex.globalIdentifierMap >>= writeIORef session.interpreter.identifierMap
            xs <- (Set.\\ moduleIndex.globalModules) <$> Session.modules session
            atomicWriteIORef moduleIndex.modules xs
            for_ xs \ mod -> do
              y <- Session.browse session mod

              let
                insert :: Text -> AvailableImports -> AvailableImports
                insert name = Map.insertWith (++) name [(Module $ T.pack mod)]

                insertAll :: AvailableImports -> AvailableImports
                insertAll m = foldr insert m y

              modifyIORef session.interpreter.identifierMap insertAll

          let
            t = Double.toFixed 2 dt
          echo $ "" <> T.unpack t <> "s"
          echo $ withColor Green " ✔\n"

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
