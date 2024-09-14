module Main (main) where

import           System.Exit
import           System.Environment
import           Control.Monad
import qualified Data.ByteString.Lazy as L

import           Client

main :: IO ()
main = do
  (success, output) <- getArgs >>= client ""
  L.putStr output
  unless success exitFailure
