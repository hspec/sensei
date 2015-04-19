module EventQueue (
  EventQueue
, newQueue
, emitEvent
, emitDone
, processQueue
) where

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Monad.STM
import           Control.Concurrent.STM.TChan

type EventQueue = TChan Event

data Event = Event | Done
  deriving Eq

newQueue :: IO EventQueue
newQueue = atomically $ newTChan

emitEvent :: EventQueue -> IO ()
emitEvent chan = atomically $ writeTChan chan Event

emitDone :: EventQueue -> IO ()
emitDone chan = atomically $ writeTChan chan Done

readEvents :: EventQueue -> IO [Event]
readEvents chan = do
  x <- atomically $ readTChan chan
  threadDelay 100000
  xs <- atomically $ emptyQueue
  return (x : xs)
  where
    emptyQueue :: STM [Event]
    emptyQueue = do
      mx <- tryReadTChan chan
      case mx of
        Nothing -> return []
        Just x -> (x :) <$> emptyQueue

processQueue :: EventQueue -> IO () -> IO ()
processQueue chan action = do
  emitEvent chan
  go
  where
    go = do
      events <- readEvents chan
      if Done `elem` events
        then return ()
        else action >> go
