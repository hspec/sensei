module Main (main) where

import           System.Environment

import           Paths_sensei
import           Run

main :: IO ()
main = do
  startupFile <- getDataFileName "startup.ghci"
  getArgs >>= run startupFile
