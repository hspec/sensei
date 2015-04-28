{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Prelude ()
import           Prelude.Compat
import           Control.Exception
import           Control.Concurrent
import           Control.Monad (void, forever)
import           Data.Foldable
import           System.FSNotify
import           Filesystem.Path.CurrentOS (encodeString)

import qualified Interpreter
import qualified HTTP

import           Util
import           Trigger
import           EventQueue

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: EventQueue -> IO ()
watchFiles queue = void . forkIO $ do
  withManager $ \manager -> do
    _ <- watchTree manager "." (not . isBoring . eventPath) (\event -> emitEvent (Just . encodeString $ eventPath event) queue)
    waitForever

watchInput :: EventQueue -> IO ()
watchInput queue = void . forkIO $ do
  input <- getContents
  forM_ (lines input) $ \_ -> do
    emitEvent Nothing queue
  emitDone queue

run :: [String] -> IO ()
run args = do
  queue <- newQueue
  watchFiles queue
  watchInput queue
  lastOutput <- newMVar (True, "")
  HTTP.withServer (readMVar lastOutput) $ do
    bracket (Interpreter.new args) Interpreter.close $ \session -> do
      let triggerAction = modifyMVar_ lastOutput $ \_ -> trigger session
      triggerAction
      processQueue queue triggerAction

runWeb :: [String] -> IO ()
runWeb args = do
  bracket (Interpreter.new args) Interpreter.close $ \session -> do
    _ <- trigger session
    lock <- newMVar ()
    HTTP.withServer (withMVar lock $ \() -> trigger session) $ do
      waitForever
