module Client (client) where

import           Imports

import           Network.Socket
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (Connection, Response(..))
import qualified Data.ByteString.Lazy as L

import           HTTP (newSocket, socketName)

client :: FilePath -> IO (Bool, L.ByteString)
client dir = handleSocketFileDoesNotExist name $ do
  manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
  Response{..} <- httpLbs "http://localhost/" manager
  return (statusIsSuccessful responseStatus, responseBody)
  where
    name :: FilePath
    name = socketName dir

    newConnection :: Maybe HostAddress -> String -> Int -> IO Connection
    newConnection _ _ _ = do
      sock <- newSocket
      connect sock (SockAddrUnix name)
      socketConnection sock 8192

handleSocketFileDoesNotExist :: String -> IO (Bool, L.ByteString) -> IO (Bool, L.ByteString)
handleSocketFileDoesNotExist name = fmap (either id id) . tryJust doesNotExist
  where
    doesNotExist :: HttpException -> Maybe (Bool, L.ByteString)
    doesNotExist = \ case
      HttpExceptionRequest _ (ConnectionFailure e) | isDoesNotExistException e -> Just (False, "could not connect to " <> fromString name <> "\n")
      _ -> Nothing

    isDoesNotExistException :: SomeException -> Bool
    isDoesNotExistException = maybe False isDoesNotExistError . fromException
