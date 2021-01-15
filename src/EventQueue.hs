{-# LANGUAGE LambdaCase #-}
module EventQueue (
  EventQueue
, newQueue

, Event(..)
, emitEvent

, processQueue

-- exported for testing
, Action(..)
, processEvents
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

data Action = NoneAction | TriggerAction | TriggerAllAction | DoneAction
  deriving (Eq, Show)

processEvents :: [Event] -> IO Action
processEvents = \ case
  events | Done `elem` events -> return DoneAction
  events | TriggerAll `elem` events -> do
    return TriggerAllAction
  events -> do
    files <- filterGitIgnoredFiles $ (nub . sort) [p | FileEvent p <- events]
    if (not $ null files) then do
      withInfoColor $ do
        mapM_ putStrLn (map ("--> " ++) files)
      return TriggerAction
    else do
      return NoneAction

processQueue :: EventQueue -> IO () -> IO () -> IO ()
processQueue chan triggerAll trigger = go
  where
    go = readEvents chan >>= processEvents >>= \ case
      NoneAction -> go
      TriggerAction -> trigger >> go
      TriggerAllAction -> triggerAll >> go
      DoneAction -> return ()
