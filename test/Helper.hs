{-# LANGUAGE QuasiQuotes #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, module System.IO.Silently
, withInterpreter
, withSomeSpec
, someSpec
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
withSomeSpec = (inTempDirectory .  (writeFile "Spec.hs" someSpec >>))

someSpec :: String
someSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]
|]

failingSpec :: String
failingSpec = [i|
module Spec (spec) where

import           Test.Hspec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      23 `shouldBe` (42 :: Int)
|]
