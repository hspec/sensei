module Helper (
  module Imports
, ghciConfig
, withSession
, withTempDirectory
, withSomeSpec
, passingSpec
, passingMetaSpec
, failingSpec
, Status(..)
, modulesLoaded
) where

import           Imports

import           System.Directory as Imports
import           System.IO.Temp (withSystemTempDirectory)
import           System.IO.Silently as Imports
import           System.FilePath as Imports ((</>))
import           System.Process as Imports (readProcess, callCommand)
import           Test.Hspec as Imports
import           Test.Mockery.Directory as Imports
import           Test.Hspec.Contrib.Mocks.V1 as Imports

import           Run ()
import qualified Session
import           Session (Session)
import           Language.Haskell.GhciWrapper (Config(..))

startupFile :: FilePath
startupFile = "startup.ghci"

ghciConfig :: Config
ghciConfig = Config {
  configIgnoreDotGhci = True
, configVerbose = False
, configStartupFile = startupFile
, configWorkingDirectory = Nothing
}

withSession :: [String] -> (Session -> IO a) -> IO a
withSession args = Session.withSession ghciConfig $ "-fhide-source-paths" : args

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory = withSystemTempDirectory "hspec"

withSomeSpec :: IO a -> IO a
withSomeSpec action = do
  startup <- readFile startupFile
  inTempDirectory $ do
    writeFile startupFile startup
    writeFile "Spec.hs" passingSpec
    action

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
