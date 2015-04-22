{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Prelude hiding (FilePath)
import           Control.Exception
import           System.Console.ANSI
import           Filesystem.Path
import           Filesystem.Path.CurrentOS () -- https://github.com/fpco/haskell-filesystem/issues/11

withInfoColor :: IO a -> IO a
withInfoColor = bracket_ set reset
  where
    set = setSGR [SetColor Foreground Dull Magenta]
    reset = setSGR []

isBoring :: FilePath -> Bool
isBoring p = ".git/" `elem` dirs || "dist/" `elem` dirs
  where
    dirs = splitDirectories p
