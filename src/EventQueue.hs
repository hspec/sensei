{-# LANGUAGE LambdaCase #-}
module EventQueue (
  EventQueue
, newQueue
, emitTriggerAll
, emitModified
, emitDone
, processQueue
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.List.Compat

import           Util

type EventQueue = TChan Event

data Event = TriggerAll | Modified FilePath | Done
  deriving Eq

newQueue :: IO EventQueue
newQueue = atomically $ newTChan

emitTriggerAll :: EventQueue -> IO ()
emitTriggerAll chan = atomically $ writeTChan chan TriggerAll

emitModified :: FilePath -> EventQueue -> IO ()
emitModified path chan = atomically $ writeTChan chan (Modified path)

emitDone :: EventQueue -> IO ()
emitDone chan = atomically $ writeTChan chan Done

readEvents :: EventQueue -> IO [Event]
readEvents chan = do
  e <- atomically $ readTChan chan
  unless (isKeyboardInput e) $ do
    threadDelay 100000
  es <- atomically emptyQueue
  return (e : es)
  where
    isKeyboardInput :: Event -> Bool
    isKeyboardInput event = event == Done || event == TriggerAll

    emptyQueue :: STM [Event]
    emptyQueue = do
      mEvent <- tryReadTChan chan
      case mEvent of
        Nothing -> return []
        Just e -> (e :) <$> emptyQueue

processQueue :: EventQueue -> IO () -> IO () -> IO ()
processQueue chan triggerAll trigger = go
  where
    go = do
      readEvents chan >>= \case
        events | Done `elem` events -> return ()
        events | TriggerAll `elem` events -> do
          triggerAll
          go
        events -> do
          let files = (nub . sort) [p | Modified p <- events]
          withInfoColor $ do
            mapM_ putStrLn (map ("--> " ++) files)
          trigger
          go
