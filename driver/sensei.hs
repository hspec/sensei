module Main (main) where

import           Prelude

import           System.Environment

import           Run

main :: IO ()
main = getArgs >>= run
