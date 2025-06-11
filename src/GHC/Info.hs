module GHC.Info (
  sensei_ghc
, Info(..)
, info
) where

import Imports

import System.Process (readProcess)
import System.Environment (getEnvironment)

sensei_ghc :: String
sensei_ghc = "SENSEI_GHC"

data Info = Info {
  ghc :: FilePath
, ghcVersion :: Version
, ghcVersionString :: String
, supportsIdeInfo :: Bool
, supportsDiagnosticsAsJson :: Bool
, globalPackageDb :: FilePath
} deriving (Eq, Show)

info :: IO Info
info = do
  ghc <- fromMaybe "ghc" . lookup sensei_ghc <$> getEnvironment

  let
    command :: (String, [String])
    command = (ghc, ["--info"])

    commandString :: String
    commandString = mconcat ["`", unwords $ uncurry (:) command, "`"]

  readMaybe <$> uncurry readProcess command "" >>= \ case
    Nothing -> die $ unwords ["could not parse the output of", commandString]
    Just values -> do
      let
        getValue :: String -> IO String
        getValue key = case lookup key values of
          Nothing -> die $ unwords [commandString, "did not return a value for", show key]
          Just value -> return value

      globalPackageDb <- getValue "Global Package DB"
      ghcVersionString <- getValue "Project version"

      ghcVersion <- case parseVersion ghcVersionString of
        Nothing -> die $ unwords [
            "could not parse GHC version"
          , show ghcVersionString
          , "obtained from", commandString
          ]
        Just ghcVersion -> return ghcVersion

      let
        supportsIdeInfo :: Bool
        supportsIdeInfo
          | ghcVersion >= makeVersion [8,8] = True
          | otherwise = False

        supportsDiagnosticsAsJson :: Bool
        supportsDiagnosticsAsJson
          | ghcVersion >= makeVersion [9,10] = True
          | otherwise = False

      return Info {
        ghc
      , ghcVersion
      , ghcVersionString
      , supportsIdeInfo
      , supportsDiagnosticsAsJson
      , globalPackageDb
      }
