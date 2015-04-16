{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Control.Exception
import           Control.Concurrent
import           Control.Monad (void, forever)
import           Data.Foldable
import           Data.List
import           Data.Time.Clock
import           System.FSNotify (withManager, watchTree)

import           Interpreter (Session)
import qualified Interpreter
import qualified Http

import           EventQueue

waitForever :: IO ()
waitForever = forever $ threadDelay 10000000

watchFiles :: EventQueue -> IO ()
watchFiles queue = void . forkIO $ do
  withManager $ \manager -> do
    _ <- watchTree manager "." (const True) (const $ emitEvent queue)
    waitForever

watchInput :: EventQueue -> IO ()
watchInput queue = void . forkIO $ do
  input <- getContents
  forM_ (lines input) $ \_ -> do
    emitEvent queue
  emitDone queue

run :: [String] -> IO ()
run args = do
  queue <- newQueue
  watchFiles queue
  watchInput queue
  lastOutput <- newMVar ""
  Http.start (readMVar lastOutput)
  bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
    processQueue queue $ modifyMVar lastOutput $ \_ -> do
      threadDelay 100000
      t <- getCurrentTime
      output <- trigger interpreter
      return (output, t)

runWeb :: [String] -> IO ()
runWeb args = do
  bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
    lock <- newMVar ()
    Http.start $ withMVar lock $ \() -> trigger interpreter
    waitForever

trigger :: Session -> IO String
trigger interpreter = do
  xs <- Interpreter.reload interpreter
  ys <- if "Ok, modules loaded:" `isInfixOf` xs
    then Interpreter.hspec interpreter
    else return ""
  return (xs ++ ys)
