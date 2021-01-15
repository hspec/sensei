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
import           Session (Session)

import           EventQueue
import           Trigger
import           Util

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: EventQueue -> IO ()
watchFiles queue = do
  watch $ emitEvent queue . \ case
    Added file _ _ -> FileEvent file
    Removed file _ _ -> FileEvent file
    Modified file _ _ -> FileEvent file
    Unknown file _ _ -> FileEvent file
  where
    isInteresting = (&&) <$> not . eventIsDirectory <*> not . isBoring . eventPath

    watch action = void . forkIO $ do
      withManager $ \ manager -> do
        _stopListening <- watchTree manager "." isInteresting action
        waitForever


watchInput :: EventQueue -> IO ()
watchInput queue = void . forkIO $ do
  input <- getContents
  forM_ (lines input) $ \_ -> do
    emitEvent queue TriggerAll
  emitEvent queue Done

run :: [String] -> IO ()
run args = do
  queue <- newQueue
  watchFiles queue
  watchInput queue
  lastOutput <- newMVar (True, "")
  HTTP.withServer (readMVar lastOutput) $ do
    let
      saveOutput :: IO (Bool, String) -> IO ()
      saveOutput action = modifyMVar_ lastOutput $ \_ -> action
    withSession args $ \ session -> do
      let
        triggerAction = saveOutput (trigger session)
        triggerAllAction = saveOutput (triggerAll session)
      triggerAction
      processQueue queue triggerAllAction triggerAction

runWeb :: [String] -> IO ()
runWeb args = do
  withSession args $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer (withMVar lock $ \() -> trigger session) $ do
      waitForever

withSession :: [String] -> (Session -> IO a) -> IO a
withSession args action = do
  check <- dotGhciWritableByOthers
  when check $ do
    putStrLn ".ghci is writable by others, you can fix this with:"
    putStrLn ""
    putStrLn "    chmod go-w .ghci ."
    putStrLn ""
    exitFailure
  bracket (Session.new args) Session.close action
