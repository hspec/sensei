module EventQueue (
  EventQueue
, newQueue
, emitEvent
, emitDone
, processQueue
) where

import           Data.List
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Monad.STM
import           Control.Concurrent.STM.TChan

import           Util

type EventQueue = TChan Event

data Event = Event (Maybe FilePath) | Done
  deriving Eq

newQueue :: IO EventQueue
newQueue = atomically $ newTChan

emitEvent :: Maybe FilePath -> EventQueue -> IO ()
emitEvent path chan = atomically $ writeTChan chan (Event path)

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
  emitEvent Nothing chan
  go
  where
    go = do
      events <- readEvents chan
      if Done `elem` events
        then return ()
        else do
          let files = (nub . sort) [p | Event (Just p) <- events]
          withInfoColor $ do
            mapM_ putStrLn (map ("--> " ++) files)
          action
          go
