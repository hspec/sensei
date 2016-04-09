{-# LANGUAGE QuasiQuotes #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, module System.IO.Silently
, withSession
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

import           Run ()
import qualified Session
import           Session (Session)

withSession :: [String] -> [String] -> (Session -> IO a) -> IO a
withSession ghciArgs hspecArgs action = bracket (Session.new ("-ignore-dot-ghci" : ghciArgs) hspecArgs) Session.close action

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

failingSpec :: String
failingSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  it "foo" True
  it "bar" False
|]
