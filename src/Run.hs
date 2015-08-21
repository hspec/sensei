{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Prelude ()
import           Prelude.Compat
import           Control.Exception
import           Control.Concurrent
import           Control.Monad (void, forever)
import           Data.Foldable
import           System.FSNotify

import qualified Session
import qualified HTTP

import           Util
import           Trigger
import           EventQueue

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: EventQueue -> IO ()
watchFiles queue = void . forkIO $ do
  withManager $ \manager -> do
    _ <- watchTree manager "." (not . isBoring . eventPath) (\event -> emitModified (eventPath event) queue)
    waitForever

watchInput :: EventQueue -> IO ()
watchInput queue = void . forkIO $ do
  input <- getContents
  forM_ (lines input) $ \_ -> do
    emitTriggerAll queue
  emitDone queue

run :: [String] -> IO ()
run args = do
  queue <- newQueue
  watchFiles queue
  watchInput queue
  lastOutput <- newMVar (True, "")
  HTTP.withServer (readMVar lastOutput) $ do
    bracket (Session.new args) Session.close $ \session -> do
      let saveOutput :: IO (Bool, String) -> IO ()
          saveOutput action = modifyMVar_ lastOutput $ \_ -> action
          triggerAction = saveOutput (trigger session)
          triggerAllAction = saveOutput (triggerAll session)
      triggerAction
      processQueue queue triggerAllAction triggerAction

runWeb :: [String] -> IO ()
runWeb args = do
  bracket (Session.new args) Session.close $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer (withMVar lock $ \() -> trigger session) $ do
      waitForever
