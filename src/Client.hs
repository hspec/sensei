{-# LANGUAGE RecordWildCards #-}
module Client (client) where

import           Prelude ()
import           Prelude.Compat
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Lazy as L

import           HTTP (newSocket, socketAddr)

client :: IO (Bool, L.ByteString)
client = withManager defaultManagerSettings {managerRawConnection = return newConnection} $ \manager -> do
  request <- parseUrl "http://localhost/"
  Response{..} <- httpLbs request {checkStatus = \_ _ _ -> Nothing} manager
  return (statusIsSuccessful responseStatus, responseBody)
  where
    newConnection _ _ _ = do
      sock <- newSocket
      connect sock socketAddr
      socketConnection sock 8192

socketConnection :: Socket -> Int -> IO Connection
socketConnection sock chunksize = makeConnection (recv sock chunksize) (sendAll sock) (sClose sock)
