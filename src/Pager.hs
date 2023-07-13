module Pager where

import           Imports

import           System.IO
import           System.Process
import           Control.Concurrent.Async

pager :: String -> IO (IO ())
pager input = do
  pid <- newEmptyMVar
  tid <- async $ withLockedHandle stdin $ do
    (Just hin, Nothing, Nothing, p) <- createProcess less { delegate_ctlc = True, std_in = CreatePipe }
    hPutStr hin input >> hClose hin
    putMVar pid p
    waitForProcess p
  return $ do
    readMVar pid >>= terminateProcess
    void $ wait tid
  where
    less = proc "less" $ "--RAW" : "--QUIT-AT-EOF" : matchOptions

matchOptions :: [String]
matchOptions = ["--incsearch", "--pattern", "^.*\\w:\\d+:\\d+:.+$|"]
