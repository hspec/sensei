{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Imports (
  module Imports
, Generic
, ToJSON(..)
, FromJSON(..)
) where

import           Prelude as Imports hiding (putStr, putStrLn, span, mod, head)
import           Control.Arrow as Imports ((>>>), (&&&))
import           Control.Concurrent as Imports
import           Control.Exception as Imports hiding (handle)
import           Control.Monad as Imports
import           Data.Function as Imports (fix)
import           Control.Applicative as Imports
import           Data.IORef as Imports hiding (modifyIORef, atomicModifyIORef)
import           Data.Functor as Imports ((<&>), ($>))
import           Data.Foldable as Imports
import           Data.Traversable as Imports
import           Data.Bifunctor as Imports
import           Data.Char as Imports
import           Data.Either as Imports
import           Data.List as Imports hiding (insert, span, head)
import           Data.Maybe as Imports
import           Data.String as Imports
import           Data.ByteString.Char8 as Imports (ByteString)
import           Data.ByteString.Lazy as Imports (LazyByteString)
import           Data.Tuple as Imports
import           Data.Set as Imports (Set)
import           Data.String.ANSI.Strip as Imports (stripAnsi)
import           System.FilePath as Imports hiding (addExtension, combine)
import           System.IO.Error as Imports (isDoesNotExistError)
import           Text.Read as Imports (readMaybe)
import           System.Exit as Imports (ExitCode(..))
import           Control.Monad.IO.Class as Imports
import           Data.Version as Imports (Version(..), showVersion, makeVersion)
import           Data.Text as Imports (Text, pack, unpack)

import           Data.Text.IO.Utf8 (hPutStrLn)
import           System.Exit (exitFailure)
import           System.Environment (getProgName)
import           System.Process as Process
import           System.IO (Handle, stderr)
import           GHC.IO.Handle.Internals (wantReadableHandle_)
import           GHC.Generics

import qualified Data.Version as Version
import           Text.ParserCombinators.ReadP

import qualified Data.Text.Encoding as T

import           Text.Casing
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           System.Clock

newtype KebabOptions a = KebabOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (KebabOptions a) where
  parseJSON = fmap KebabOptions . genericKebabDecode

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (KebabOptions a) where
  toJSON (KebabOptions a) = genericKebabEncode a

genericKebabDecode :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
genericKebabDecode = genericParseJSON kebabAesonOptions

genericKebabEncode :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
genericKebabEncode = genericToJSON kebabAesonOptions

kebabAesonOptions :: Options
kebabAesonOptions = defaultOptions {
  fieldLabelModifier = kebab
, rejectUnknownFields = True
}

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
encodeUtf8 = T.encodeUtf8 . pack

decodeUtf8 :: ByteString -> String
decodeUtf8 = unpack . T.decodeUtf8Lenient

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseVersion :: String -> Maybe Version
parseVersion xs = case [v | (v, "") <- readP_to_S Version.parseVersion xs] of
  [v] -> Just v
  _ -> Nothing

head :: [a] -> Maybe a
head = listToMaybe

data These a b = This a | That b | These a b

theseFromMaybes :: Maybe a -> Maybe b -> Maybe (These a b)
theseFromMaybes ma mb = case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just $ This a
  (Nothing, Just b) -> Just $ That b
  (Just a, Just b) -> Just $ These a b

this :: These a b -> Maybe a
this = \ case
  This a -> Just a
  That _ -> Nothing
  These a _ -> Just a

that :: These a b -> Maybe b
that = \ case
  This _ -> Nothing
  That b -> Just b
  These _ b -> Just b

atomicReadIORef :: IORef a -> IO a
atomicReadIORef ref = atomicModifyIORef' ref (id &&& id)

data GHC =
    ANY
  | GHC_904
  | GHC_906
  | GHC_908
  | GHC_910
  | GHC_912
  deriving (Eq, Ord, Bounded)

requiredFor :: GHC -> a -> a
requiredFor _ = id
{-# INLINE requiredFor #-}

timeAction :: MonadIO m => m a -> m (Double, a)
timeAction action = do
  start <- liftIO $ getTime Monotonic
  result <- action
  end <- liftIO $ getTime Monotonic
  let dt = fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9
  return (dt, result)

data TerminateProcess = TerminateProcess String
  deriving (Eq, Show)

instance Exception TerminateProcess

handleTerminateProcess :: IO a -> IO a
handleTerminateProcess action = try action >>= either terminate return
  where
    terminate :: TerminateProcess -> IO a
    terminate (TerminateProcess err) = do
      name <- getProgName
      hPutStrLn stderr . pack $ name <> ": " <> err
      exitFailure

die :: String -> IO a
die err = throwIO $ TerminateProcess err
