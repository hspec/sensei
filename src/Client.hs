{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Client (client) where

import           Imports

import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (Response(..))
import           Network.HTTP.Types
import           Network.Socket (connect)
import qualified Data.ByteString.Lazy as L

import           HTTP (newSocket, socketAddr, socketName)

client :: FilePath -> IO (Bool, L.ByteString)
client dir = fromRight connectError <$> tryJust p go
  where
    connectError :: (Bool, L.ByteString)
    connectError = (False, "could not connect to " <> fromString (socketName dir) <> "\n")

    p :: HttpException -> Maybe ()
    p e = case e of
      HttpExceptionRequest _ (ConnectionFailure se) -> guard (isDoesNotExistException se) >> Just ()
      _ -> Nothing

    isDoesNotExistException :: SomeException -> Bool
    isDoesNotExistException = maybe False isDoesNotExistError . fromException

    go = do
      manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
      Response{..} <- httpLbs "http://localhost/" manager
      return (statusIsSuccessful responseStatus, responseBody)

    newConnection _ _ _ = do
      sock <- newSocket
      connect sock $ socketAddr dir
      socketConnection sock 8192
