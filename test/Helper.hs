{-# LANGUAGE QuasiQuotes #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, module System.IO.Silently
, withInterpreter
, withSomeSpec
, passingSpec
, failingSpec
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Control.Exception
import           Control.Applicative
import           System.IO.Silently
import           Data.String.Interpolate

import           Interpreter (Session)
import qualified Interpreter

withInterpreter :: [String] -> (Session -> IO a) -> IO a
withInterpreter args action = bracket (Interpreter.new $ "-ignore-dot-ghci" : args) Interpreter.close action

withSomeSpec :: IO a -> IO a
withSomeSpec = (inTempDirectory .  (writeFile "Spec.hs" passingSpec >>))

passingSpec :: String
passingSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  it "foo" $ do
    23 `shouldBe` (23 :: Int)

  it "bar" $ do
    42 `shouldBe` (42 :: Int)
|]

failingSpec :: String
failingSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  it "foo" $ do
    23 `shouldBe` (23 :: Int)

  it "bar" $ do
    23 `shouldBe` (42 :: Int)
|]
