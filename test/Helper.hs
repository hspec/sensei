{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, module System.IO.Silently
, withSession
, withSenseiSession
, withSomeSpec
, passingSpec
, failingSpec
) where

import           Control.Applicative
import           Control.Exception
import           Data.String.Interpolate
import           System.IO.Silently
import           Test.Hspec
import           Test.Mockery.Directory

import           Options
import           Run                     ()
import           Session                 (Session)
import qualified Session

withSession :: [String] -> (Session -> IO a) -> IO a
withSession args action = bracket (Session.new $ SenseiArgs "^*$" True ("-ignore-dot-ghci" : args)) Session.close action

withSomeSpec :: IO a -> IO a
withSomeSpec = (inTempDirectory .  (writeFile "Spec.hs" passingSpec >>))

withSenseiSession :: SenseiArgs -> (Session -> IO a) -> IO a
withSenseiSession SenseiArgs{..} action = bracket (Session.new $ SenseiArgs watch testFlag ("-ignore-dot-ghci" : otherArgs)) Session.close action

passingSpec :: String
passingSpec = [i|
module Spec (spec) where

import           Test.Hspec

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
