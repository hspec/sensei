{-# LANGUAGE CPP #-}
{-# LANGUAGE NoFieldSelectors #-}
module Run (
  run
, runWeb
#ifdef TEST
, RunArgs(..)
, runWith
, defaultRunArgs
#endif
) where

import           Imports

import qualified Data.ByteString as B
import           System.IO
import qualified System.FSNotify as FSNotify

import qualified HTTP
import qualified Session
import           Session (withSession)

import           EventQueue
import           Trigger
import           Util

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: FilePath -> EventQueue -> IO ()
watchFiles dir queue = do
  watch $ \ case
    FSNotify.Added file _ _ -> emit $ FileEvent FileAdded file
    FSNotify.Modified file _ _ -> emit $ FileEvent FileModified file
    FSNotify.ModifiedAttributes _file _ _ -> pass
    FSNotify.Removed file _ _ -> emit $ FileEvent FileRemoved file
    FSNotify.WatchedDirectoryRemoved _file _ _ -> pass
    FSNotify.CloseWrite file _ _ -> emit $ FileEvent FileModified file
    FSNotify.Unknown file _ _ _ -> emit $ FileEvent FileModified file
  where
    emit :: EventQueue.Event -> IO ()
    emit = emitEvent queue

    watch :: FSNotify.Action -> IO ()
    watch action = void . forkIO $ do
      FSNotify.withManager $ \ manager -> do
        _stopListening <- FSNotify.watchTree manager dir isInteresting action
        waitForever

    isInteresting :: FSNotify.Event -> Bool
    isInteresting = (&&) <$> isFile <*> not . isBoring . FSNotify.eventPath

    isFile :: FSNotify.Event -> Bool
    isFile = FSNotify.eventIsDirectory >>> (== FSNotify.IsFile)

watchInput :: EventQueue -> IO ()
watchInput queue = void . forkIO $ do
  input <- getContents
  forM_ (lines input) $ \_ -> do
    emitEvent queue TriggerAll
  emitEvent queue Done

run :: FilePath -> [String] -> IO ()
run startupFile args = do
  runArgs@RunArgs{dir, lastOutput, queue} <- defaultRunArgs startupFile
  watchFiles dir queue
  watchInput queue
  HTTP.withServer dir (readMVar lastOutput) $ do
    runWith runArgs {args}

defaultRunArgs :: FilePath -> IO RunArgs
defaultRunArgs startupFile = do
  queue <- newQueue
  lastOutput <- newMVar (Trigger.Success, "")
  return RunArgs {
    dir = ""
  , startupFile
  , args = []
  , lastOutput = lastOutput
  , queue = queue
  }

data RunArgs = RunArgs {
  dir :: FilePath
, startupFile :: FilePath
, args :: [String]
, lastOutput :: MVar (Result, String)
, queue :: EventQueue
}

runWith :: RunArgs -> IO ()
runWith RunArgs {..} = fix $ \ rec -> do
  status <- withSession (sessionConfig startupFile) args $ \ session -> do
    let
      triggerAction = saveOutput (trigger session)
      triggerAllAction = saveOutput (triggerAll session)
    triggerAction
    processQueue dir queue triggerAllAction triggerAction
  case status of
    Restart -> rec
    Terminate -> return ()
  where
    saveOutput :: IO (Trigger.Result, String) -> IO ()
    saveOutput action = modifyMVar_ lastOutput $ \ _ -> action

runWeb :: FilePath -> [String] -> IO ()
runWeb startupFile args = do
  withSession (sessionConfig startupFile) args $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer "" (withMVar lock $ \() -> trigger session) $ do
      waitForever

sessionConfig :: FilePath -> Session.Config
sessionConfig startupFile = Session.Config {
  configIgnoreDotGhci = False
, configStartupFile = startupFile
, configWorkingDirectory = Nothing
, configEcho = \ string -> B.putStr string >> hFlush stdout
}
