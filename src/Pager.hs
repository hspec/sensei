module Pager where

import           Imports

import           System.IO
import           System.IO.Error
import           System.Process
import           Control.Concurrent.Async

pager :: String -> IO (IO ())
pager = pagerWith less
  where
    less = proc "less" $ "--RAW" : "--QUIT-AT-EOF" : matchOptions

matchOptions :: [String]
matchOptions = ["--incsearch", "--pattern", "^.*\\w:\\d+:\\d+:.+$|"]

pagerWith :: CreateProcess -> String -> IO (IO ())
pagerWith process input = do
  pid <- newEmptyMVar
  tid <- async $ withLockedHandle stdin $ do
    (Just hin, Nothing, Nothing, p) <- createProcess process { delegate_ctlc = True, std_in = CreatePipe }
    putMVar pid p
    _ <- tryJust (guard . isResourceVanishedError) $ hPutStr hin input >> hClose hin
    waitForProcess p
  return $ do
    readMVar pid >>= terminateProcess
    void $ wait tid
