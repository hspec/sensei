{-# LANGUAGE CPP #-}
module GHC.EnvironmentFile (
  listAllHieFiles
#ifdef TEST
, readPackageConfig
, determineHieDirectory
, listHieFiles
, Entry(..)
, parseEntries
#endif
) where

import Util
import Distribution.Text (display)
import Prelude (putStrLn)
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription
import Data.ByteString qualified as B
import Distribution.InstalledPackageInfo
import System.Directory
import System.Environment.Blank (getEnv)
import Data.Text.IO.Utf8 qualified as Text
import Data.Text qualified as T
import           Prelude ()
import           Imports

import qualified GHC.Info as GHC (Info(..))

warning :: String -> IO ()
warning = putStrLn . withColor Red

listHieFiles :: FilePath -> InstalledPackageInfo -> IO [FilePath]
listHieFiles dir package = catMaybes <$> do
  for package.exposedModules \ case
    ExposedModule name Nothing -> do
      let p = dir </> toFilePath name <.> "hie"
      doesFileExist p >>= \ case
        False -> do
          warning $ "non-existing " <> p
          return Nothing
        True -> do
          return $ Just p
    ExposedModule name (Just xx) -> do
      let
        packageName :: String
        packageName = unPackageName . pkgName $ sourcePackageId package
      -- e.g.
      --
      -- GHC.Num.BigNat from ghc-bignum-1.3-32a7:GHC.Num.BigNat
      warning $ "ignoring re-export " <> display xx <> " as " <> packageName <> ":" <> display name
      return Nothing

listAllHieFiles :: GHC.Info -> IO [FilePath]
listAllHieFiles info = do
  boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> info.ghcVersionString)

  xs <- getPackages info.globalPackageDb
  concat <$> for xs \ x -> do
    determineHieDirectory boot x >>= \ case
      Nothing -> return []
      Just dir -> listHieFiles dir x

determineHieDirectory :: FilePath -> InstalledPackageInfo -> IO (Maybe FilePath)
determineHieDirectory boot x = do
  let
    packageName :: String
    packageName = unPackageName . pkgName $ sourcePackageId x

  r <- firstDir $ (libraryDirs x <&> \ name -> name </> "extra-compilation-artifacts" </> "hie") ++ [boot </> packageName]
  case r of
    Just p -> do
      return $ Just p
    Nothing -> do
      warning $ "no HIE directory for " <> display (installedComponentId x)
      return Nothing

firstDir :: [FilePath] -> IO (Maybe FilePath)
firstDir = \ case
  [] -> return Nothing
  dir : dirs -> do
    doesDirectoryExist dir >>= \ case
      True -> return $ Just dir
      False -> firstDir dirs

getPackages :: FilePath -> IO [InstalledPackageInfo]
getPackages globalPackageDb = listPackageConfigs globalPackageDb >>= traverse readPackageConfig

readPackageConfig :: FilePath -> IO InstalledPackageInfo
readPackageConfig name = parseInstalledPackageInfo <$> B.readFile name >>= \ case
  Left err -> die . unlines $ (unwords ["Reading", name, "failed!"]) : "" : toList err
  Right (_, package) -> return package { libraryDirs = map expand package.libraryDirs }
  where
    expand :: FilePath -> FilePath
    expand = T.unpack . T.replace "${pkgroot}" (T.pack . takeDirectory $ takeDirectory name) . T.pack

listPackageConfigs :: FilePath -> IO [FilePath]
listPackageConfigs globalPackageDb = do
  entries <- parseGhcEnvironment
  let
    dbs = globalPackageDb : [db | PackageDb db <- entries]
    packages = [db | PackageId db <- entries]
  catMaybes <$> for packages \ package -> findFile dbs (T.unpack package <> ".conf")

parseGhcEnvironment :: IO [Entry]
parseGhcEnvironment = getEnv "GHC_ENVIRONMENT" >>= \ case
  Nothing -> return []
  Just name -> Text.readFile name <&> parseEntries name >>= \ case
    Left err -> die err
    Right entries -> return entries

data Entry =
    ClearPackageDb
  | GlobalPackageDb
  | UserPackageDb
  | PackageDb FilePath
  | PackageId Text
  | HidePackage Text
  deriving (Eq, Show)

parseEntries :: FilePath -> Text -> Either String [Entry]
parseEntries file = traverse (parseEntry file) . discardComments . T.lines

discardComments :: [Text] -> [Text]
discardComments = filter (not . T.isPrefixOf "--")

parseEntry :: FilePath -> Text -> Either String Entry
parseEntry envfile str = case T.words str of
  ["clear-package-db"] -> return ClearPackageDb
  ["global-package-db"] -> return GlobalPackageDb
  ["user-package-db"] -> return UserPackageDb
  ("package-db": _) -> return (PackageDb (envdir </> T.unpack db))
    -- relative package dbs are interpreted relative to the env file
    where envdir = takeDirectory envfile
          db = T.drop 11 str
  ["package-id", packageId] -> return $ PackageId packageId
  ["hide-package", package] -> return $ HidePackage package
  [packageId] -> return $ PackageId packageId
  _ -> Left $ "Can't parse environment file entry: " ++ envfile ++ ": " ++ T.unpack str
