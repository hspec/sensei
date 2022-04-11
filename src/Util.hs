{-# LANGUAGE CPP, OverloadedStrings, LambdaCase, RecordWildCards, ViewPatterns #-}
module Util (
  Color(..)
, withColor
, withInfoColor
, isBoring
, filterGitIgnoredFiles
, normalizeTypeSignatures
, dotGhciWritableByOthers

#ifdef TEST
, filterGitIgnoredFiles_
, gitCheckIgnoreFeedback
, writableByOthers
#endif
) where

import           Imports

import           System.Console.ANSI
import           System.FilePath
import           System.Process
import           System.Posix.Files
import           System.Posix.Types
import qualified Data.Text as T

withInfoColor :: IO a -> IO a
withInfoColor = withColor Magenta

withColor :: Color -> IO a -> IO a
withColor c = bracket_ set reset
  where
    set = setSGR [SetColor Foreground Dull c]
    reset = setSGR []

isBoring :: FilePath -> Bool
isBoring p = ".git" `elem` dirs || "dist" `elem` dirs || isEmacsAutoSave p
  where
    dirs = splitDirectories p
    isEmacsAutoSave = isPrefixOf ".#" . takeFileName

filterGitIgnoredFiles :: [FilePath] -> IO [FilePath]
filterGitIgnoredFiles files = do
  (feedback, ignoredFiles) <- filterGitIgnoredFiles_ files
  printFeedback feedback
  return $ ignoredFiles
  where
    printFeedback :: Feedback -> IO ()
    printFeedback = mapM_ $ \ (color, err) -> withColor color $ putStrLn ('\n' : err)

type Feedback = Maybe (Color, String)

filterGitIgnoredFiles_ :: [FilePath] -> IO (Feedback, [FilePath])
filterGitIgnoredFiles_ files = fmap (files \\) <$> gitCheckIgnore files

gitCheckIgnore :: [FilePath] -> IO (Feedback, [FilePath])
gitCheckIgnore files = do
  (_, ignoredFiles, err) <- readProcessWithExitCode "git" ["check-ignore", "--stdin", "-z"] $ join_ files
  return (gitCheckIgnoreFeedback err, split ignoredFiles)
  where
    join_ = intercalate "\0"
    split = map T.unpack . T.split (== '\0') . T.pack

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
    normalize = \case
      '\n' : ' ' : ' ' : xs -> normalizeTypeSignatures (' ' : dropWhile (== ' ') xs)
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
