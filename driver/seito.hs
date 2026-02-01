module Main (main) where

import           Prelude

import           System.Exit
import           System.Environment
import           Control.Monad
import qualified Data.ByteString.Lazy as L

import           Paths_sensei (getDataFileName)

import           Client

main :: IO ()
main = do
  (success, output) <- getArgs >>= client getDataFileName ""
  L.putStr output
  unless success exitFailure
