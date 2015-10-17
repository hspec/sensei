{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Util where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Data.List.Compat
import           System.Console.ANSI
import           System.FilePath
import           System.Posix.Files
import           System.Posix.Types

withInfoColor :: IO a -> IO a
withInfoColor = bracket_ set reset
  where
    set = setSGR [SetColor Foreground Dull Magenta]
    reset = setSGR []

isBoring :: FilePath -> Bool
isBoring p = ".git" `elem` dirs || "dist" `elem` dirs || isEmacsAutoSave p
  where
    dirs = splitDirectories p
    isEmacsAutoSave = isPrefixOf ".#" . takeBaseName

normalizeTypeSignatures :: String -> String
normalizeTypeSignatures = \case
  xs | "\n  :: " `isPrefixOf` xs -> normalizeTypeSignatures (drop 2 xs)
  x : xs -> x : normalizeTypeSignatures xs
  [] -> []

dotGhciWritableByOthers :: IO Bool
dotGhciWritableByOthers = do
  exists <- fileExist ".ghci"
  if exists then do
    mode <- fileMode <$> getFileStatus ".ghci"
    dirMode <- fileMode <$> getFileStatus "."
    return (writableByOthers mode || writableByOthers dirMode)
  else
    return False

writableByOthers :: FileMode -> Bool
writableByOthers mode = m /= nullFileMode
  where
    m = intersectFileModes (unionFileModes otherWriteMode groupWriteMode) mode
