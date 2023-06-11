module Helper (
  module Imports
, silent
, ghciConfig
, withTempDirectory
, withSomeSpec
, passingSpec
, passingMetaSpec
, failingSpec
, Status(..)
, modulesLoaded

, Color(..)
, withColor

, timeout
) where

import           Imports

import           System.Directory as Imports
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process as Imports (readProcess, callProcess, callCommand)
import           Test.Hspec as Imports
import           Test.Hspec.Contrib.Mocks.V1 as Imports
import           Test.Mockery.Directory as Imports (touch)

import           System.Environment
import qualified System.Timeout

import           Run ()
import           Util
import           Language.Haskell.GhciWrapper (Config(..))

timeout :: IO a -> IO (Maybe a)
timeout action = lookupEnv "CI" >>= \ case
  Nothing -> System.Timeout.timeout  5_000_000 action
  Just _  -> System.Timeout.timeout 30_000_000 action

silent :: a -> IO ()
silent _ = pass

ghciConfig :: Config
ghciConfig = Config {
  configIgnoreDotGhci = True
, configStartupFile = "startup.ghci"
, configWorkingDirectory = Nothing
, configEcho = silent
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

data Status = Ok | Failed
  deriving (Eq, Show)

modulesLoaded :: Status -> [String] -> String
modulesLoaded status xs = show status ++ ", " ++ mods ++ " loaded."
  where
    n = length xs
    mods
      | n == 0 = "no modules"
      | n == 1 = "one module"
      | n == 2 = "two modules"
      | n == 3 = "three modules"
      | n == 4 = "four modules"
      | n == 5 = "five modules"
      | n == 6 = "six modules"
      | otherwise = show n ++ " modules"
