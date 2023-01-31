{-# LANGUAGE LambdaCase #-}
module Run (
  run
, runWeb
) where

import           Imports

import           System.Exit
import           System.FSNotify

import qualified HTTP
import qualified Session
import           Session (Session, Config(..))

import           EventQueue
import           Trigger
import           Util

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: EventQueue -> IO ()
watchFiles queue = do
  watch $ \ case
    Added file _ _ -> emit $ FileEvent FileAdded file
    Modified file _ _ -> emit $ FileEvent FileModified file
    ModifiedAttributes _file _ _ -> pass
    Removed file _ _ -> emit $ FileEvent FileRemoved file
    WatchedDirectoryRemoved _file _ _ -> pass
    CloseWrite file _ _ -> emit $ FileEvent FileModified file
    Unknown file _ _ _ -> emit $ FileEvent FileModified file
  where
    emit = emitEvent queue

    watch action = void . forkIO $ do
      withManager $ \ manager -> do
        _stopListening <- watchTree manager "." isInteresting action
        waitForever
      where
        isInteresting = (&&) <$> isFile <*> not . isBoring . eventPath
        isFile = eventIsDirectory >>> (== IsFile)

watchInput :: EventQueue -> IO ()
watchInput queue = void . forkIO $ do
  input <- getContents
  forM_ (lines input) $ \_ -> do
    emitEvent queue TriggerAll
  emitEvent queue Done

run :: FilePath -> [String] -> IO ()
run startupFile args = do
  queue <- newQueue
  watchFiles queue
  watchInput queue
  lastOutput <- newMVar (True, "")
  HTTP.withServer (readMVar lastOutput) $ do
    let
      saveOutput :: IO (Bool, String) -> IO ()
      saveOutput action = modifyMVar_ lastOutput $ \_ -> action

      go = do
        status <- withSession startupFile args $ \ session -> do
          let
            triggerAction = saveOutput (trigger session)
            triggerAllAction = saveOutput (triggerAll session)
          triggerAction
          processQueue queue triggerAllAction triggerAction
        case status of
          Reload -> go
          Terminate -> return ()
    go

runWeb :: FilePath -> [String] -> IO ()
runWeb startupFile args = do
  withSession startupFile args $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer (withMVar lock $ \() -> trigger session) $ do
      waitForever

withSession :: FilePath -> [String] -> (Session -> IO a) -> IO a
withSession startupFile args action = do
  check <- dotGhciWritableByOthers
  when check $ do
    putStrLn ".ghci is writable by others, you can fix this with:"
    putStrLn ""
    putStrLn "    chmod go-w .ghci ."
    putStrLn ""
    exitFailure
  Session.withSession config args action
  where
    config :: Config
    config = Config {
      configIgnoreDotGhci = False
    , configVerbose = True
    , configStartupFile = startupFile
    }
