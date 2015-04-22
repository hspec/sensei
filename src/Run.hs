{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Control.Exception
import           Control.Concurrent
import           Control.Monad (void, forever)
import           Data.Foldable
import           Data.List
import           System.FSNotify
import           Filesystem.Path.CurrentOS (encodeString)

import           Interpreter (Session)
import qualified Interpreter
import qualified Http

import           Util
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

socketName :: String
socketName = ".autospec.sock"

reportSocketName :: IO ()
reportSocketName = withInfoColor $ putStrLn ("listening on " ++ socketName)

run :: [String] -> IO ()
run args = do
  queue <- newQueue
  watchFiles queue
  watchInput queue
  lastOutput <- newMVar ""
  reportSocketName
  void . forkIO $ Http.start socketName (readMVar lastOutput)
  bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
    processQueue queue $ modifyMVar_ lastOutput $ \_ -> trigger interpreter

runWeb :: [String] -> IO ()
runWeb args = do
  bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
    _ <- trigger interpreter
    lock <- newMVar ()
    reportSocketName
    Http.start socketName $ withMVar lock $ \() -> trigger interpreter

trigger :: Session -> IO String
trigger interpreter = do
  xs <- Interpreter.reload interpreter
  ys <- if "Ok, modules loaded:" `isInfixOf` xs
    then Interpreter.hspec interpreter
    else return ""
  return (xs ++ ys)
