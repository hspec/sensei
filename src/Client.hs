module Client (client) where

import Imports

import System.IO
import Network.HTTP.Client

import HTTP.Util (makeRequest)

client :: FilePath -> [String] -> IO (Bool, LazyByteString)
client dir args = case args of
  [] -> hIsTerminalDevice stdout >>= run
  ["--no-color"] -> run False
  ["--color"] -> run True
  _ -> do
    hPutStrLn stderr $ "Usage: seito [ --color | --no-color ]"
    return (False, "")
  where
    run :: Bool -> IO (Bool, LazyByteString)
    run color = do
      let
        url :: Request
        url = fromString $ "http://localhost/?color=" <> map toLower (show color)
      makeRequest dir url
