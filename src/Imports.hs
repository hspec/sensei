{-# LANGUAGE CPP #-}
module Imports (module Imports) where

import           Control.Arrow as Imports ((>>>), (&&&))
import           Control.Concurrent as Imports
import           Control.Exception as Imports hiding (handle)
import           Control.Monad as Imports
import           Data.Function as Imports (fix)
import           Control.Applicative as Imports
import           Data.Functor as Imports ((<&>))
import           Data.Bifunctor as Imports
import           Data.Char as Imports
import           Data.Either as Imports
import           Data.List as Imports hiding (span)
import           Data.Maybe as Imports
import           Data.String as Imports
import           Data.ByteString.Char8 as Imports (ByteString, pack, unpack)
import           Data.Tuple as Imports
import           System.FilePath as Imports hiding (combine)
import           System.IO.Error as Imports (isDoesNotExistError)
import           Text.Read as Imports (readMaybe)
import           System.Exit as Imports (ExitCode(..))
import           System.Process as Process
import           Control.Monad.IO.Class as Imports

import           System.IO (Handle)
import           GHC.IO.Handle.Internals (wantReadableHandle_)

import           Data.Version as Imports (Version(..), showVersion, makeVersion)
import qualified Data.Version as Version
import           Text.ParserCombinators.ReadP

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

pass :: Applicative m => m ()
pass = pure ()

while :: Monad m => m Bool -> m () -> m ()
while p action = go
  where
    go = do
      notDone <- p
      when notDone $ do
        action
        go

withLockedHandle :: Handle -> IO a -> IO a
withLockedHandle h = wantReadableHandle_ "pager" h . const

createPipe :: IO (Handle, Handle)
#if defined(__IO_MANAGER_WINIO__)
#error Use `associateHandle'` as per https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html#v:createPipe
#endif
createPipe = Process.createPipe

encodeUtf8 :: String -> ByteString
encodeUtf8 = T.encodeUtf8 . T.pack

decodeUtf8 :: ByteString -> String
decodeUtf8 = T.unpack . T.decodeUtf8Lenient

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseVersion :: String -> Maybe Version
parseVersion xs = case [v | (v, "") <- readP_to_S Version.parseVersion xs] of
  [v] -> Just v
  _ -> Nothing
