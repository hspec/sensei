{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Helper (
  module Imports
, encodeUtf8
, decodeUtf8
, ensureFile

, md5sum
, tryReadFile

, shouldBe
, shouldReturn

, describe_
) where

import System.IO.Unsafe (unsafePerformIO)
import           Imports

import qualified Language.Haskell.TH.Syntax as TH

import           Control.Exception as Imports
import           System.IO.Error as Imports (isDoesNotExistError)

import           Test.Hspec as Imports hiding (shouldBe, shouldReturn)
import qualified Test.Hspec as Hspec
import           Test.Hspec.Contrib.Mocks.V1 as Imports
import           Test.Mockery.Directory as Imports (touch)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as T
import           System.Directory
import           System.Process as Imports (readProcess, callProcess, callCommand)

import           Foreign (withForeignPtr)
import           GHC.Fingerprint
import           Data.ByteString.Internal (ByteString(..))

import           GHC.Diagnostic.Annotated

encodeUtf8 :: String -> ByteString
encodeUtf8 = T.encodeUtf8 . pack

decodeUtf8 :: ByteString -> String
decodeUtf8 = unpack . T.decodeUtf8Lenient

md5sum :: ByteString -> ByteString
md5sum (BS p n) = encodeUtf8 . show . unsafePerformIO $ withForeignPtr p $ flip fingerprintData n

tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile = fmap (either (const Nothing) Just) . tryJust (guard . isDoesNotExistError) . ByteString.readFile

ensureFile :: FilePath -> ByteString -> IO ()
ensureFile name new = do
  createDirectoryIfMissing True $ takeDirectory name
  old <- tryReadFile name
  unless (old == Just new) $ do
    ByteString.writeFile name new

infixr 0 `shouldBe`, `shouldReturn`

shouldBe :: HasCallStack => (Eq a, Show a) => a -> a -> IO ()
shouldBe = Hspec.shouldBe

shouldReturn :: HasCallStack => (Eq a, Show a) => IO a -> a -> IO ()
shouldReturn = Hspec.shouldReturn

describe_ :: TH.Name -> SpecWith a -> SpecWith a
describe_ = describe . TH.nameBase

instance IsString RequiredVariable where
  fromString name = RequiredVariable Unqualified (fromString name) NoTypeSignature

instance IsString Qualification where
  fromString = Qualified . fromString

instance IsString TypeSignature where
  fromString = TypeSignature . fromString

instance IsString Module where
  fromString = Module "base" . fromString

instance IsString Package where
  fromString = Package DirectDependency . fromString
