{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Helper (
  module Imports

, whenGhc

, encodeUtf8
, decodeUtf8
, ensureFile
, getCacheDirectory

, shouldBe
, shouldReturn

, describe_
) where

import           Imports

import qualified Language.Haskell.TH.Syntax as TH

import           Control.Exception as Imports
import           System.IO.Error as Imports (isDoesNotExistError)

import           Test.Hspec as Imports hiding (shouldBe, shouldReturn)
import qualified Test.Hspec as Hspec
import           Test.Hspec.Contrib.Mocks.V1 as Imports
import           Test.Mockery.Directory as Imports (touch)

import           System.Environment
import           System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as T
import           System.Directory
import           System.Process as Imports (readProcess, callProcess, callCommand)

import           GHC.Diagnostic.Annotated

whenGhc :: GHC -> IO () -> IO ()
whenGhc required action = case required of
  ANY -> action
  GHC_910 -> action
  GHC_912 -> do
#if __GLASGOW_HASKELL__ >= 912
    action
#else
    pass
#endif

encodeUtf8 :: String -> ByteString
encodeUtf8 = T.encodeUtf8 . pack

decodeUtf8 :: ByteString -> String
decodeUtf8 = unpack . T.decodeUtf8Lenient

tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile = fmap (either (const Nothing) Just) . tryJust (guard . isDoesNotExistError) . ByteString.readFile

ensureFile :: FilePath -> ByteString -> IO ()
ensureFile name new = do
  createDirectoryIfMissing True $ takeDirectory name
  old <- tryReadFile name
  unless (old == Just new) $ do
    ByteString.writeFile name new

getCacheDirectory :: IO String
getCacheDirectory = lookupEnv "SENSEI_TEST_CACHE" >>= \ case
  Nothing -> do
    tmp <- getCanonicalTemporaryDirectory
    dir <- createTempDirectory tmp "sensei-tests"
    setEnv "SENSEI_TEST_CACHE" dir
    return dir
  Just dir -> do
    return dir

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
