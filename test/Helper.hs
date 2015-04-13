module Helper (
  module Test.Hspec
, module Control.Applicative
, module System.IO.Silently
, withInterpreter
) where

import           Test.Hspec
import           Control.Exception
import           Control.Applicative
import           System.IO.Silently

import           Interpreter (Interpreter)
import qualified Interpreter

withInterpreter :: [String] -> (Interpreter -> IO a) -> IO a
withInterpreter args action = bracket (Interpreter.new $ "-ignore-dot-ghci" : args) Interpreter.close action
