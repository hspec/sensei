module HTTP.Util where

import           Imports

import           Network.Socket
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (Connection, Response(..))
import qualified Data.ByteString.Lazy as L

socketName :: FilePath -> String
socketName dir = dir </> ".sensei.sock"

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

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
