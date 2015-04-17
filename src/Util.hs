{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Prelude hiding (FilePath)
import           Filesystem.Path
import           Filesystem.Path.CurrentOS () -- https://github.com/fpco/haskell-filesystem/issues/11

isBoring :: FilePath -> Bool
isBoring p = ".git/" `elem` dirs || "dist/" `elem` dirs
  where
    dirs = splitDirectories p
