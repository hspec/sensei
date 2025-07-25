{-# LANGUAGE CPP #-}
module Util (
  Color(..)
, withColor
, withInfoColor
, isBoring
, filterGitIgnoredFiles
, normalizeTypeSignatures
, isWritableByOthers

#ifdef TEST
, filterGitIgnoredFiles_
, gitCheckIgnoreFeedback
, writableByOthers
#endif
) where

import           Imports

import           System.Console.ANSI
import           System.Process
import           System.Posix.Files
import           System.Posix.Types
import qualified Data.Text as T

withInfoColor :: String -> String
withInfoColor = withColor Magenta

withColor :: Color -> String -> String
withColor color string =  set <> string <> reset
  where
    set = setSGRCode [SetColor Foreground Dull color]
    reset = setSGRCode []

isBoring :: FilePath -> Bool
isBoring p = ".git" `elem` dirs || "dist" `elem` dirs || isEmacsAutoSave p
  where
    dirs = splitDirectories p
    isEmacsAutoSave = isPrefixOf ".#" . takeFileName

filterGitIgnoredFiles :: (String -> IO ()) -> FilePath -> [FilePath] -> IO [FilePath]
filterGitIgnoredFiles echo dir files = do
  (feedback, ignoredFiles) <- filterGitIgnoredFiles_ dir files
  printFeedback feedback
  return ignoredFiles
  where
    printFeedback :: Feedback -> IO ()
    printFeedback = mapM_ $ \ (color, err) -> echo ('\n' : withColor color err <> "\n")

type Feedback = Maybe (Color, String)

filterGitIgnoredFiles_ :: FilePath -> [FilePath] -> IO (Feedback, [FilePath])
filterGitIgnoredFiles_ dir files = fmap (files \\) <$> gitCheckIgnore dir files

gitCheckIgnore :: FilePath -> [FilePath] -> IO (Feedback, [FilePath])
gitCheckIgnore dir files = do
  (_, ignoredFiles, err) <- readProcessWithExitCode "git" ["-C", dir, "check-ignore", "--stdin", "-z"] $ join_ files
  return (gitCheckIgnoreFeedback err, split ignoredFiles)
  where
    join_ :: [String] -> String
    join_ = intercalate "\0"

    split :: String -> [String]
    split = map unpack . T.split (== '\0') . pack

gitCheckIgnoreFeedback :: String -> Feedback
gitCheckIgnoreFeedback err
  | "fatal: not a git repository (or any " `isPrefixOf` err = notGitWarning
  | err == "" = Nothing
  | otherwise = Just (Red, err)
  where
    notGitWarning = Just (Cyan, "warning: not a git repository - .gitignore support not available\n")

normalizeTypeSignatures :: String -> String
normalizeTypeSignatures = normalize . concatMap replace
  where
    normalize :: [Char] -> [Char]
    normalize = \case
      '\n' : ' ' : ' ' : xs -> normalize (' ' : dropWhile (== ' ') xs)
      x : xs -> x : normalize xs
      [] -> []

    replace :: Char -> [Char]
    replace c = case c of
      '\8759' -> "::"
      '\8594' -> "->"
      _ -> [c]

isWritableByOthers :: FilePath -> IO Bool
isWritableByOthers name = do
  exists <- fileExist name
  if exists then do
    mode <- fileMode <$> getFileStatus name
    dirMode <- fileMode <$> getFileStatus (takeDirectory name)
    return (writableByOthers mode || writableByOthers dirMode)
  else
    return False

writableByOthers :: FileMode -> Bool
writableByOthers mode = m /= nullFileMode
  where
    m = intersectFileModes (unionFileModes otherWriteMode groupWriteMode) mode
