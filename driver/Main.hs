module Main (main) where

import           System.Environment

import           Run

main :: IO ()
main = do
  args <- getArgs
  getContents >>= run args
