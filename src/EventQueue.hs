{-# LANGUAGE LambdaCase #-}
module EventQueue (
  EventQueue
, newQueue

, Event(..)
, emitEvent

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

data Event = TriggerAll | FileEvent FilePath | Done
  deriving Eq

newQueue :: IO EventQueue
newQueue = atomically $ newTChan

emitEvent :: EventQueue -> Event -> IO ()
emitEvent chan = atomically . writeTChan chan

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
          files <- filterGitIgnoredFiles $ (nub . sort) [p | FileEvent p <- events]
          unless (null files) $ do
            withInfoColor $ do
              mapM_ putStrLn (map ("--> " ++) files)
            trigger
          go
