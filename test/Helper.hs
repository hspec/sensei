{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, module System.IO.Silently
, withSession
, withSomeSpec
, passingSpec
, passingMetaSpec
, failingSpec
, Status(..)
, modulesLoaded
) where

import           Control.Applicative
import           Control.Exception
import           Data.String.Interpolate
#if __GLASGOW_HASKELL__ < 802
import           Data.List.Compat
#endif
import           System.IO.Silently
import           Test.Hspec
import           Test.Mockery.Directory

import           Run ()
import qualified Session
import           Session (Session)

withSession :: [String] -> (Session -> IO a) -> IO a
withSession args action = bracket (Session.new $ "-ignore-dot-ghci" : args) Session.close action

withSomeSpec :: IO a -> IO a
withSomeSpec = (inTempDirectory .  (writeFile "Spec.hs" passingSpec >>))

passingSpec :: String
passingSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  it "foo" True
  it "bar" True
|]

passingMetaSpec :: String
passingMetaSpec = [i|
module Spec (spec) where

import           Test.Hspec.Meta

spec :: Spec
spec = do
  it "foo" True
  it "bar" True
|]

failingSpec :: String
failingSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  it "foo" True
  it "bar" False
|]

data Status = Ok | Failed
  deriving (Eq, Show)

modulesLoaded :: Status -> [String] -> String
#if __GLASGOW_HASKELL__ < 802
modulesLoaded status xs = show status ++ ", modules loaded: " ++ mods ++ "."
  where
    mods = case xs of
      [] -> "none"
      _ -> intercalate ", " xs
#else
modulesLoaded status xs = show status ++ ", " ++ show n ++ " " ++ mods ++ " loaded."
  where
    n = length xs
    mods
      | n == 1 = "module"
      | otherwise = "modules"
#endif
