module Client (client) where

import Imports

import System.IO
import Network.HTTP.Client

import HTTP.Util (makeRequest)
import Sensei.API qualified as API

client :: (FilePath -> IO FilePath) -> FilePath -> [String] -> IO (Bool, LazyByteString)
client getDataFileName dir args = case args of
  [] -> hIsTerminalDevice stdout >>= run
  ["--no-color"] -> run False
  ["--color"] -> run True
  ["trigger"] -> API.trigger dir
  ["--vim-config"] -> (,) True . fromString <$> getDataFileName "vim/sensei.vim"
  _ -> do
    hPutStrLn stderr $ "Usage: seito [ --color | --no-color ]"
    hPutStrLn stderr $ "       seito trigger"
    hPutStrLn stderr $ "       seito --vim-config"
    return (False, "")
  where
    run :: Bool -> IO (Bool, LazyByteString)
    run color = do
      let
        url :: Request
        url = fromString $ "http://localhost/?color=" <> map toLower (show color)
      makeRequest dir url
