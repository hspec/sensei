{-# LANGUAGE LambdaCase #-}
module Run (
  run
, runWeb
) where

import           Imports

import qualified Data.ByteString as B
import           System.IO
import           System.Exit
import qualified System.FSNotify as FSNotify

import qualified HTTP
import qualified Session
import           Session (Session, Config(..))

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

run :: FilePath -> FilePath -> [String] -> IO ()
run dir startupFile args = do
  queue <- newQueue
  watchFiles dir queue
  watchInput queue
  lastOutput <- newMVar (Trigger.Success, "")
  HTTP.withServer dir (readMVar lastOutput) $ do
    let
      saveOutput :: IO (Trigger.Result, String) -> IO ()
      saveOutput action = modifyMVar_ lastOutput $ \ _ -> action

      go = do
        status <- withSession startupFile args $ \ session -> do
          let
            triggerAction = saveOutput (trigger session)
            triggerAllAction = saveOutput (triggerAll session)
          triggerAction
          processQueue dir queue triggerAllAction triggerAction
        case status of
          Restart -> go
          Terminate -> return ()
    go

runWeb :: FilePath -> [String] -> IO ()
runWeb startupFile args = do
  withSession startupFile args $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer "" (withMVar lock $ \() -> trigger session) $ do
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
    , configStartupFile = startupFile
    , configWorkingDirectory = Nothing
    , configEcho = \ string -> B.putStr string >> hFlush stdout
    }
