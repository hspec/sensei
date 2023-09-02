module Input (watch) where

import           Imports

import           System.IO
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

watch :: Handle -> (Char -> IO ()) -> IO () -> IO ()
watch handle dispatch onEof = void . forkIO $ do
  forEachInputChar handle dispatch `finally` onEof

forEachInputChar :: Handle -> (Char -> IO ()) -> IO ()
forEachInputChar handle action = do
  hSetBuffering handle NoBuffering
  hSetEcho handle False
  go
  where
    chunkSize :: Int
    chunkSize = 64

    inputDelay :: Int
    inputDelay = 100_000

    go :: IO ()
    go = ByteString.hGetNonBlocking handle chunkSize >>= \ case
      "" -> threadDelay inputDelay >> go
      cs -> consume (Char8.unpack cs)

    consume :: [Char] -> IO ()
    consume = \ case
      c : _ | eof c -> pass
      c : cs -> do
        when (isAscii c) (action c)
        consume cs
      [] -> go

    eof :: Char -> Bool
    eof = (== '\EOT')
