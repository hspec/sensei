module EventQueue (
  EventQueue
, newQueue
, emitTrigger
, emitModified
, emitDone
, processQueue
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.List.Compat

import           Util

type EventQueue = TChan Event

data Event = Trigger | Modified FilePath | Done
  deriving Eq

newQueue :: IO EventQueue
newQueue = atomically $ newTChan

emitTrigger :: EventQueue -> IO ()
emitTrigger chan = atomically $ writeTChan chan Trigger

emitModified :: FilePath -> EventQueue -> IO ()
emitModified path chan = atomically $ writeTChan chan (Modified path)

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
processQueue chan action = go
  where
    go = do
      events <- readEvents chan
      if Done `elem` events
        then return ()
        else do
          let files = (nub . sort) [p | Modified p <- events]
          withInfoColor $ do
            mapM_ putStrLn (map ("--> " ++) files)
          action
          go
