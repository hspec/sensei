module GHC.Environment where

import Util
import Distribution.Text (display)
import Prelude (putStrLn)
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription
import Data.ByteString qualified as B
import           Distribution.InstalledPackageInfo as C
import System.Process (readProcess)
import System.Directory
import System.Environment.Blank (getEnv)
import Data.Text.IO.Utf8 qualified as Text
import Data.Text qualified as T
import           Prelude ()
import           Imports

data Entry = 
    ClearPackageDb
  | GlobalPackageDb
  | UserPackageDb
  | PackageDb FilePath
  | PackageId Text
  | HidePackage Text
  deriving (Eq, Show)

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

listAllHieFiles :: IO [FilePath]
listAllHieFiles = do
  let version = "9.10.1"
  boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> version)

  xs <- getPackages
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


getPackages :: IO [InstalledPackageInfo]
getPackages = listPackageConfigs >>= traverse readPackageConfig

readPackageConfig :: FilePath -> IO InstalledPackageInfo
readPackageConfig p = do
  C.parseInstalledPackageInfo <$> B.readFile p >>= \ case
    Left _err -> fail "XXX todo"
    Right (_, r) -> return r

listPackageConfigs :: IO [FilePath]
listPackageConfigs = do
  info <- readProcess "ghc" ["--info"] ""

  let
    globalPackageDb :: Maybe FilePath
    globalPackageDb = readMaybe info >>= lookup @String "Global Package DB"

  entries <- parseGhcEnvironment
  let
    dbs = maybe id (:) globalPackageDb $ [db | PackageDb db <- entries]

    packages = [db | PackageId db <- entries]

  catMaybes <$> for packages \ package -> findFile dbs (T.unpack package <> ".conf")

parseGhcEnvironment :: IO [Entry]
parseGhcEnvironment = do
  getEnv "GHC_ENVIRONMENT" >>= \ case
    Nothing -> return []
    Just name -> Text.readFile name <&> (T.lines >>> mapMaybe (parseEntry name))

parseEntry :: FilePath -> Text -> Maybe Entry
parseEntry envfile str = case T.words str of
  ["clear-package-db"] -> Just ClearPackageDb
  ["global-package-db"] -> Just GlobalPackageDb
  ["user-package-db"] -> Just UserPackageDb
  ("package-db": _) -> Just (PackageDb (envdir </> T.unpack db))
    -- relative package dbs are interpreted relative to the env file
    where envdir = takeDirectory envfile
          db = T.drop 11 str
  ["package-id", packageId] -> Just $ PackageId packageId
  ["hide-package", package] -> Just $ HidePackage package
  ((T.unpack -> ('-':'-':_)):_) -> Nothing
  [packageId] -> Just $ PackageId packageId
  _ -> Nothing
