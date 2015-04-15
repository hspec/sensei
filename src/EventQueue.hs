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
emitEvent chan = (Event <$> getCurrentTime) >>= writeChan chan

emitDone :: EventQueue -> IO ()
emitDone chan = writeChan chan Done

processQueue :: EventQueue -> IO () -> IO ()
processQueue chan action = do
  emitEvent chan
  go (posixSecondsToUTCTime 0)
  where
    go t0 = do
      event <- readChan chan
      case event of
        Done -> return ()
        Event t | t0 < t -> do
          threadDelay 100000
          t1 <- getCurrentTime
          action >> go t1
        Event _ -> go t0
