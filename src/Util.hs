{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Util where

import           Control.Exception
import           Data.List
import           System.Console.ANSI
import           System.FilePath

withInfoColor :: IO a -> IO a
withInfoColor = bracket_ set reset
  where
    set = setSGR [SetColor Foreground Dull Magenta]
    reset = setSGR []

isBoring :: FilePath -> Bool
isBoring p = ".git" `elem` dirs || "dist" `elem` dirs
  where
    dirs = splitDirectories p

normalizeTypeSignatures :: String -> String
normalizeTypeSignatures = \case
  xs | "\n  :: " `isPrefixOf` xs -> normalizeTypeSignatures (drop 2 xs)
  x : xs -> x : normalizeTypeSignatures xs
  [] -> []
