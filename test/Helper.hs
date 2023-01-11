{-# LANGUAGE QuasiQuotes #-}
module Helper (
  module Imports
, withSession
, withSomeSpec
, passingSpec
, passingMetaSpec
, failingSpec
, Status(..)
, modulesLoaded
) where

import           Imports

import           System.IO.Silently as Imports
import           System.Process as Imports (readProcess, callCommand)
import           Test.Hspec as Imports
import           Test.Mockery.Directory as Imports

import           Run ()
import qualified Session
import           Session (Session)

withSession :: [String] -> (Session -> IO a) -> IO a
withSession args = bracket (Session.new $ "-ignore-dot-ghci" : args) Session.close

withSomeSpec :: IO a -> IO a
withSomeSpec = inTempDirectory .  (writeFile "Spec.hs" passingSpec >>)

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
