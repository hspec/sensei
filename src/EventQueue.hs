module EventQueue where

import           Control.Concurrent
import           Control.Applicative
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

type EventQueue = Chan Event

data Event = Event UTCTime | Done

newQueue :: IO EventQueue
newQueue = newChan

emitEvent :: EventQueue -> IO ()
emitEvent queue = (Event <$> getCurrentTime) >>= writeChan queue

emitDone :: EventQueue -> IO ()
emitDone queue = writeChan queue Done

processQueue :: EventQueue -> IO UTCTime -> IO ()
processQueue queue action = do
  emitEvent queue
  go (posixSecondsToUTCTime 0)
  where
    go t0 = do
      event <- readChan queue
      case event of
        Done -> return ()
        Event t | t0 < t -> action >>= go
        Event _ -> go t0
