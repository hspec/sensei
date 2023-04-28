{-# LANGUAGE OverloadedStrings #-}
module Helper (
  module Imports
, ghciConfig
, withTempDirectory
, withSomeSpec
, passingSpec
, passingMetaSpec
, failingSpec
, Status(..)
, modulesLoaded
, randomChunkSizes
) where

import           Imports

import           System.Directory as Imports
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process as Imports (readProcess, callProcess, callCommand)
import qualified Data.ByteString as B
import           Test.Hspec as Imports
import           Test.Hspec.Contrib.Mocks.V1 as Imports
import           Test.QuickCheck as Imports hiding (output)

import           Run ()
import           Language.Haskell.GhciWrapper (Config(..))

ghciConfig :: Config
ghciConfig = Config {
  configIgnoreDotGhci = True
, configStartupFile = "startup.ghci"
, configWorkingDirectory = Nothing
, configEcho = \ _ -> pass
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

data ChunkSizes = SmallChunks | BigChunks

randomChunkSizes :: ByteString -> Gen [ByteString]
randomChunkSizes input = do
  chunkSizes <- elements [SmallChunks, BigChunks]
  let
    maxChunkSize = case chunkSizes of
      SmallChunks -> 4
      BigChunks -> B.length input
  chunkByteString (1, maxChunkSize) input

chunkByteString :: (Int, Int) -> ByteString -> Gen [ByteString]
chunkByteString size = go
  where
    go "" = return []
    go xs = do
      n <- chooseInt size
      let (chunk, rest) = B.splitAt n xs
      (chunk :) <$> go rest
