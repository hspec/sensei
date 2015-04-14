{-# LANGUAGE OverloadedStrings #-}
module Run where

import           Control.Exception
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad (void, forever)
import           Data.Foldable
import           Data.List
import           Data.Time.Clock
import           System.FSNotify (withManager, watchTree)

import           Interpreter (Interpreter)
import qualified Interpreter

data Event = Event UTCTime | Done

emitEvent :: Chan Event -> IO ()
emitEvent chan = (Event <$> getCurrentTime) >>= writeChan chan

watchFiles :: Chan Event -> IO ()
watchFiles chan = void . forkIO $ do
  withManager $ \manager -> do
    _ <- watchTree manager "." (const True) (const $ emitEvent chan)
    forever $ threadDelay maxBound

watchInput :: Chan Event -> String -> IO ()
watchInput chan input = void . forkIO $ do
  forM_ (lines input) $ \_ -> do
    emitEvent chan
  writeChan chan Done

run :: [String] -> String -> IO ()
run args input = do
  chan <- newChan
  watchFiles chan
  watchInput chan input
  bracket (Interpreter.new args) Interpreter.close (processEvents chan)

processEvents :: Chan Event -> Interpreter -> IO ()
processEvents chan interpreter = do
  t <- getCurrentTime
  emitEvent chan
  go t
  where
    go t0 = do
      event <- readChan chan
      case event of
        Done -> return ()
        Event t | t0 < t -> do
          threadDelay 100000
          t1 <- getCurrentTime
          trigger interpreter >> go t1
        Event _ -> go t0

trigger :: Interpreter -> IO String
trigger interpreter = do
  xs <- Interpreter.reload interpreter
  ys <- if "Ok, modules loaded:" `isInfixOf` xs
    then Interpreter.hspec interpreter
    else return ""
  return (xs ++ ys)
