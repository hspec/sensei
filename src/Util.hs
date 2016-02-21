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
    isEmacsAutoSave = isPrefixOf ".#" . takeFileName

normalizeTypeSignatures :: String -> String
normalizeTypeSignatures = normalize . concatMap replace
  where
    normalize = \case
      xs | "\n  :: " `isPrefixOf` xs -> normalizeTypeSignatures (drop 2 xs)
      x : xs -> x : normalizeTypeSignatures xs
      [] -> []

    replace c = case c of
      '\8759' -> "::"
      '\8594' -> "->"
      _ -> [c]

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
