module Main (main) where

import           System.Environment

import           Run

main :: IO ()
main = getArgs >>= run
