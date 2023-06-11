{-# LANGUAGE CPP #-}
module Run (
  run
, runWeb
#ifdef TEST
, RunArgs(..)
, runWith
, defaultRunArgs
, watchFiles
#endif
) where

import           Imports

import qualified Data.ByteString as B
import           Data.IORef
import           System.IO
import qualified System.FSNotify as FSNotify

import qualified HTTP
import qualified Session
import           Session (withSession)

import           EventQueue
import           Trigger
import qualified Input
import           Pager (pager)
import           Util

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: FilePath -> EventQueue -> IO () -> IO ()
watchFiles dir queue action = do
  FSNotify.withManager $ \ manager -> do
    bracket (FSNotify.watchTree manager dir isInteresting dispatch) (\ stopListening -> stopListening) $ \ _ -> do
      action
  where
    dispatch :: FSNotify.Event -> IO ()
    dispatch = \ case
      FSNotify.Added file _ _ -> emit $ FileEvent FileAdded file
      FSNotify.Modified file _ _ -> emit $ FileEvent FileModified file
      FSNotify.ModifiedAttributes file _ _ -> emit $ FileEvent FileModified file
      FSNotify.Removed file _ _ -> emit $ FileEvent FileRemoved file
      FSNotify.WatchedDirectoryRemoved _file _ _ -> pass
      FSNotify.CloseWrite file _ _ -> emit $ FileEvent FileModified file
      FSNotify.Unknown file _ _ _ -> emit $ FileEvent FileModified file

    emit :: EventQueue.Event -> IO ()
    emit = emitEvent queue

    isInteresting :: FSNotify.Event -> Bool
    isInteresting = (&&) <$> isFile <*> not . isBoring . FSNotify.eventPath

    isFile :: FSNotify.Event -> Bool
    isFile = FSNotify.eventIsDirectory >>> (== FSNotify.IsFile)

run :: FilePath -> [String] -> IO ()
run startupFile args = do
  runArgs@RunArgs{dir, lastOutput, queue} <- defaultRunArgs startupFile
  watchFiles dir queue $ do
    Input.watch stdin (dispatch queue) (emitEvent queue Done)
    HTTP.withServer dir (readMVar lastOutput) $ do
      runWith runArgs {args}
  where
    dispatch :: EventQueue -> Char -> IO ()
    dispatch queue = \ case
      '\n' -> emitEvent queue TriggerAll
      'q' -> emitEvent queue Done
      _ -> pass

defaultRunArgs :: FilePath -> IO RunArgs
defaultRunArgs startupFile = do
  queue <- newQueue
  lastOutput <- newMVar (Trigger.Success, "")
  return RunArgs {
    dir = ""
  , args = []
  , lastOutput = lastOutput
  , queue = queue
  , sessionConfig = defaultSessionConfig startupFile
  }

data RunArgs = RunArgs {
  dir :: FilePath
, args :: [String]
, lastOutput :: MVar (Result, String)
, queue :: EventQueue
, sessionConfig  :: Session.Config
}

runWith :: RunArgs -> IO ()
runWith RunArgs {..} = do
  cleanup <- newIORef pass
  let
    runCleanupAction :: IO ()
    runCleanupAction = join $ atomicModifyIORef' cleanup $ (,) pass

    addCleanupAction :: IO () -> IO ()
    addCleanupAction cleanupAction = atomicModifyIORef' cleanup $ \ action -> (action >> cleanupAction, ())

    saveOutput :: IO (Trigger.Result, String) -> IO ()
    saveOutput action = do
      runCleanupAction
      result <- modifyMVar lastOutput $ \ _ -> (id &&& id) <$> action
      case result of
        (Failure, output) -> pager output >>= addCleanupAction
        (Success, _output) -> pass

    go = do
      status <- withSession sessionConfig args $ \ session -> do
        let
          triggerAction = saveOutput (trigger session)
          triggerAllAction = saveOutput (triggerAll session)
        triggerAction
        processQueue (sessionConfig.configEcho . encodeUtf8) dir queue triggerAllAction triggerAction
      case status of
        Restart -> go
        Terminate -> return ()
  go

runWeb :: FilePath -> [String] -> IO ()
runWeb startupFile args = do
  withSession (defaultSessionConfig startupFile) args $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer "" (withMVar lock $ \() -> trigger session) $ do
      waitForever

defaultSessionConfig :: FilePath -> Session.Config
defaultSessionConfig startupFile = Session.Config {
  configIgnoreDotGhci = False
, configStartupFile = startupFile
, configWorkingDirectory = Nothing
, configEcho = \ string -> B.putStr string >> hFlush stdout
}
