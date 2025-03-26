module Client (client) where

import           Imports

import           System.IO
import           Network.Socket
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (Connection, Response(..))

import           HTTP (newSocket, socketName)

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
    run color = handleSocketFileDoesNotExist name $ do
      manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
      let
        url :: Request
        url = fromString $ "http://localhost/?color=" <> map toLower (show color)
      Response{..} <- httpLbs url manager
      return (statusIsSuccessful responseStatus, responseBody)

    name :: FilePath
    name = socketName dir

    newConnection :: Maybe HostAddress -> String -> Int -> IO Connection
    newConnection _ _ _ = do
      sock <- newSocket
      connect sock (SockAddrUnix name)
      socketConnection sock 8192

handleSocketFileDoesNotExist :: String -> IO (Bool, LazyByteString) -> IO (Bool, LazyByteString)
handleSocketFileDoesNotExist name = fmap (either id id) . tryJust doesNotExist
  where
    doesNotExist :: HttpException -> Maybe (Bool, LazyByteString)
    doesNotExist = \ case
      HttpExceptionRequest _ (ConnectionFailure e) | isDoesNotExistException e -> Just (False, "could not connect to " <> fromString name <> "\n")
      _ -> Nothing

    isDoesNotExistException :: SomeException -> Bool
    isDoesNotExistException = maybe False isDoesNotExistError . fromException
