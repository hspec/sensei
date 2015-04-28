{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Client (client) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad.Compat
import           Control.Exception
import           Data.String
import           System.IO.Error
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Lazy as L

import           HTTP (newSocket, socketAddr, socketName)

client :: IO (Bool, L.ByteString)
client = either (const $ connectError) id <$> tryJust p go
  where
    connectError :: (Bool, L.ByteString)
    connectError = (False, "could not connect to " <> fromString socketName <> "\n")

    p :: HttpException -> Maybe ()
    p e = case e of
      FailedConnectionException2 _ _ _ se -> guard (isDoesNotExistException se) >> Just ()
      _ -> Nothing

    isDoesNotExistException :: SomeException -> Bool
    isDoesNotExistException = maybe False isDoesNotExistError . fromException

    go = withManager defaultManagerSettings {managerRawConnection = return newConnection} $ \manager -> do
      request <- parseUrl "http://localhost/"
      Response{..} <- httpLbs request {checkStatus = \_ _ _ -> Nothing} manager
      return (statusIsSuccessful responseStatus, responseBody)

    newConnection _ _ _ = do
      sock <- newSocket
      connect sock socketAddr
      socketConnection sock 8192

socketConnection :: Socket -> Int -> IO Connection
socketConnection sock chunksize = makeConnection (recv sock chunksize) (sendAll sock) (sClose sock)
