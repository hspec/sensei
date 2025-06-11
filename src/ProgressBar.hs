module ProgressBar where

import Imports

progress :: Int -> Int -> String
progress total n = done <> todo <> pp
  where
    done = replicate p '▰'
    todo = replicate (10 - p) '▱'

    pp = " " <> show (p * 10) <> "%"
    p :: Int
    p = total * 10 `div` n

withProgressBar :: MonadIO m => (String -> m ()) -> [a] -> (a -> m b) -> m [b]
withProgressBar put items action = do
  let total = length items
  r <- for (zip [0..] items) \ (n, item) -> do
    put $ progress n total <> "\r"
    action item
  put $ progress 10 10 <> "\n"
  return r

withProgressBar_ :: MonadIO m => (String -> m ()) -> [a] -> (a -> m b) -> m ()
withProgressBar_ put items action = do
  let total = length items
  for_ (zip [0..] items) \ (n, item) -> do
    put $ progress n total <> "\r"
    action item
  put $ progress 10 10 <> "\n"
