{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Helper (
  module Imports
, silent
, ghciConfig

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

, to_json

, requireGhc
, ifGhc

, ensureFile
) where

import           Imports

import           System.Directory as Imports
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process as Imports (readProcess, callProcess, callCommand)
import           Test.Hspec as Imports
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

timeout :: HasCallStack => IO a -> IO a
timeout action = do
  lookupEnv "CI" >>= \ case
    Nothing -> System.Timeout.timeout  5_000_000 action
    Just _  -> System.Timeout.timeout 30_000_000 action
  >>= maybe (assertFailure "timeout exceeded") return

silent :: a -> IO ()
silent _ = pass

ghciConfig :: Config
ghciConfig = Config {
  configIgnoreDotGhci = True
, configIgnore_GHC_ENVIRONMENT = False
, configWorkingDirectory = Nothing
, configHieDirectory = Nothing
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
, ghcVersion = "ghc-" <> __GLASGOW_HASKELL_FULL_VERSION__
, span = Nothing
, severity = Error
, code = Nothing
, message = []
, hints = []
}

to_json :: ToJSON a => a -> ByteString
to_json = toStrict . encode

requireGhc :: [Int] -> IO ()
requireGhc = ifGhc >=> (`unless` pending)

ifGhc :: [Int] -> IO Bool
ifGhc (makeVersion -> required) = do
  ghcVersion <- getGhcVersion
  return (ghcVersion >= required)

getGhcVersion :: IO Version
getGhcVersion = do
  env <- getEnvironment
  let Just ghcVersion = lookupGhcVersion env >>= parseVersion
  return ghcVersion

ensureFile :: FilePath -> ByteString -> IO ()
ensureFile name new = do
  createDirectoryIfMissing True $ takeDirectory name
  old <- tryReadFile name
  unless (old == Just new) $ do
    ByteString.writeFile name new
