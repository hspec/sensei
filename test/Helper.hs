{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Helper (
  module Imports
, silent

, Info(..)
, ghcInfo
, ghciConfig
, getCacheDirectory

, AppConfig(..)
, appConfig

, withTempDirectory
, withSomeSpec
, passingSpec
, passingMetaSpec
, failingSpec

, Color(..)
, withColor

, timeout

, Annotated(..)
, Diagnostic(..)
, Annotation(..)
, Span(..)
, Location(..)
, Severity(..)
, diagnostic
, diagnosticForGhc

, to_json

, require
, ifGhc
, whenGhc

, ensureFile

, shouldBe
, shouldReturn

, describe_
) where

import           Imports

import qualified Language.Haskell.TH.Syntax as TH

import           System.Directory as Imports
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process as Imports (readProcess, callProcess, callCommand)
import           Test.Hspec as Imports hiding (shouldBe, shouldReturn)
import qualified Test.Hspec as Hspec
import           Test.Hspec.Contrib.Mocks.V1 as Imports
import           Test.Mockery.Directory as Imports (touch)
import           Test.HUnit

import           System.Environment
import qualified System.Timeout

import qualified Data.ByteString as ByteString
import           Data.ByteString.Lazy (toStrict)
import           Data.Aeson (encode)

import           Run ()
import           HTTP (AppConfig(..))
import           Config (tryReadFile)
import           Util
import           Language.Haskell.GhciWrapper
import qualified Trigger

import           GHC.Diagnostic
import           GHC.Diagnostic.Annotated

import           GHC.Info (Info(..))
import           SpecHook (ghcInfo, getCacheDirectory)

timeout :: HasCallStack => IO a -> IO a
timeout action = do
  lookupEnv "CI" >>= \ case
    Nothing -> System.Timeout.timeout  5_000_000 action
    Just _  -> System.Timeout.timeout 30_000_000 action
  >>= maybe (assertFailure "timeout exceeded") return

silent :: a -> IO ()
silent _ = pass

ghciConfig :: IO Config
ghciConfig = do
  info <- ghcInfo
  return Config {
    ghc = info.ghc
  , ignoreDotGhci = True
  , ignore_GHC_ENVIRONMENT = False
  , workingDirectory = Nothing
  , diagnosticsAsJson = info.supportsDiagnosticsAsJson
  , hieDirectory = Nothing
  , configEcho = silent
  }

appConfig :: FilePath -> HTTP.AppConfig
appConfig dir = HTTP.AppConfig {
  dir
, hieDir = "hie"
, putStrLn = \ _ -> pass
, deepSeek = Nothing
, trigger = pass
, getLastResult = return (Trigger.Success, "", [])
, getModules = return []
}

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory = withSystemTempDirectory "hspec"

withSomeSpec :: (FilePath -> IO a) -> IO a
withSomeSpec action = withTempDirectory $ \ dir -> do
  let name = dir </> "Spec.hs"
  writeFile name passingSpec
  action name

passingSpec :: String
passingSpec = unlines [
    "module Spec (spec) where"
  , ""
  , "import           Test.Hspec"
  , ""
  , "spec :: Spec"
  , "spec = do"
  , "  it \"foo\" True"
  , "  it \"bar\" True"
  ]

passingMetaSpec :: String
passingMetaSpec = unlines [
    "module Spec (spec) where"
  , ""
  , "import           Test.Hspec.Meta"
  , ""
  , "spec :: Spec"
  , "spec = do"
  , "  it \"foo\" True"
  , "  it \"bar\" True"
  ]

failingSpec :: String
failingSpec = unlines [
    "module Spec (spec) where"
  , ""
  , "import           Test.Hspec"
  , ""
  , "spec :: Spec"
  , "spec = do"
  , "  it \"foo\" True"
  , "  it \"bar\" False"
  ]

diagnostic :: Diagnostic
diagnostic = Diagnostic {
  version = "1.0"
, ghcVersion = "ghc-9.10.0"
, span = Nothing
, severity = Error
, code = Nothing
, message = []
, hints = []
, reason = Nothing
}

diagnosticForGhc :: IO Diagnostic
diagnosticForGhc = do
  info <- ghcInfo
  let
    version :: String
    version
      | info.ghcVersion <= makeVersion [9,12] = "1.0"
      | otherwise = "1.1"
  return diagnostic {
    version
  , ghcVersion = "ghc-" <> info.ghcVersionString
  }

to_json :: ToJSON a => a -> ByteString
to_json = toStrict . encode

require :: GHC -> IO ()
require = ifGhc >=> (`unless` pending)

whenGhc :: GHC -> IO () -> IO ()
whenGhc required action = ifGhc required >>= (`when` action)

ifGhc :: GHC -> IO Bool
ifGhc (toVersion -> required) = do
  info <- ghcInfo
  return (info.ghcVersion >= required)

toVersion :: GHC -> Version
toVersion = makeVersion . \ case
  ANY -> [0]
  GHC_904 -> [9,4]
  GHC_906 -> [9,6]
  GHC_908 -> [9,8]
  GHC_910 -> [9,10]
  GHC_912 -> [9,12]

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
