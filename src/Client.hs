module Client (
  client
, makeRequest
) where

import           Imports

import           System.IO
import           Network.Socket
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (Connection, Response(..))
import qualified Data.ByteString.Lazy as L

import           HTTP (newSocket, socketName)

client :: (FilePath -> IO FilePath) -> FilePath -> [String] -> IO (Bool, LazyByteString)
client getDataFileName dir args = case args of
  [] -> hIsTerminalDevice stdout >>= run
  ["--no-color"] -> run False
  ["--color"] -> run True
  ["--vim-config"] -> (,) True . fromString <$> getDataFileName "vim/sensei.vim"
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

makeRequest :: FilePath -> Request -> IO (Bool, LazyByteString)
makeRequest dir url = handleSocketFileDoesNotExist name $ do
  manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
  Response{..} <- httpLbs url manager
  return (statusIsSuccessful responseStatus, responseBody)
  where
    name :: FilePath
    name = socketName dir

    newConnection :: Maybe HostAddress -> String -> Int -> IO Connection
    newConnection _ _ _ = do
      sock <- newSocket
      connect sock (SockAddrUnix name)
      socketConnection sock 8192

handleSocketFileDoesNotExist :: FilePath -> IO (Bool, LazyByteString) -> IO (Bool, LazyByteString)
handleSocketFileDoesNotExist name = fmap (either id id) . tryJust doesNotExist
  where
    doesNotExist :: HttpException -> Maybe (Bool, LazyByteString)
    doesNotExist = \ case
      HttpExceptionRequest _ (ConnectionFailure e) | isDoesNotExistException e -> Just (False, "could not connect to " <> L.fromStrict (encodeUtf8 name) <> "\n")
      _ -> Nothing

    isDoesNotExistException :: SomeException -> Bool
    isDoesNotExistException = maybe False isDoesNotExistError . fromException
