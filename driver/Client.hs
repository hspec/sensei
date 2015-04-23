module Main (main) where

import           System.Exit
import           Control.Monad
import qualified Data.ByteString.Lazy as L

import           Client

main :: IO ()
main = do
  (success, output) <- client
  L.putStr output
  unless success exitFailure
