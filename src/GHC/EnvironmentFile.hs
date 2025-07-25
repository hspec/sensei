{-# LANGUAGE CPP #-}
module GHC.EnvironmentFile (
  Warning(..)
, HieFilePath(..)
, listAllHieFiles
#ifdef TEST
, readCabalDependencies
, readPackageConfig
, Entry(..)
, parseEntries
#endif
) where

import Imports

import Data.List.NonEmpty qualified as NonEmpty
import Data.ByteString qualified as B
import Data.Set qualified as Set
import System.Environment.Blank (getEnv)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T (readFile)
import System.Directory
import Control.Monad.Trans.Writer.CPS

import Distribution.Text (display)
import Distribution.Backpack
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.Configuration
import Distribution.InstalledPackageInfo

import GHC.Info qualified as GHC (Info(..))
import GHC.GhcPkg qualified as GhcPkg
import GHC.Diagnostic.Annotated

getCabalFiles :: FilePath -> IO [FilePath]
getCabalFiles dir = filter (not . ("." `isPrefixOf`)) . filter (".cabal" `isSuffixOf`) <$> getDirectoryContents dir

readCabalDependencies :: FilePath -> IO (Set Text)
readCabalDependencies dir = getCabalFiles dir >>= \ case
  [file] -> B.readFile file <&> parseGenericPackageDescriptionMaybe >>= \ case
    Nothing -> mempty
    Just (flattenPackageDescription -> package) -> do
      let
        libraryDependencies :: [Dependency]
        libraryDependencies  = maybe [] (targetBuildDepends . libBuildInfo) (library package)

        subLibraryDependencies :: [Dependency]
        subLibraryDependencies = concatMap targetBuildDepends $ map libBuildInfo (subLibraries package)

        executableDependencies :: [Dependency]
        executableDependencies = concatMap targetBuildDepends $ map buildInfo (executables package)

        testDependencies :: [Dependency]
        testDependencies = concatMap targetBuildDepends $ map testBuildInfo (testSuites package)

        benchmarkDependencies :: [Dependency]
        benchmarkDependencies = concatMap targetBuildDepends $ map benchmarkBuildInfo (benchmarks package)

        dependencies :: Set Text
        dependencies = Set.fromList . map (pack . unPackageName . depPkgName) $ concat [
            libraryDependencies
          , subLibraryDependencies
          , executableDependencies
          , testDependencies
          , benchmarkDependencies
          ]
      return dependencies
  _ -> mempty

type WarningM = WriterT [Warning] IO

newtype Warning = Warning String
  deriving newtype (Eq, Show)

data HieFilePath = HieFilePath {
  package :: Package
, path :: FilePath
} deriving (Eq, Show)

listAllHieFiles :: GHC.Info -> IO ([Warning], [HieFilePath])
listAllHieFiles info = do
  home <- getHomeDirectory
  let fallback = home </> ".local" </> "state" </> "ghc-hie-files" </> "ghc-" <> info.ghcVersionString
  packages <- listPackages info
  directDependencies <- readCabalDependencies "."
  swap <$> runWriterT do
    concat <$> for packages (packageHieFiles directDependencies fallback)

packageHieFiles :: Set Text -> FilePath -> InstalledPackageInfo -> WarningM [HieFilePath ]
packageHieFiles directDependencies fallback package = do
  let
    libToHie :: FilePath -> FilePath
    libToHie lib = lib </> "extra-compilation-artifacts" </> "hie"

    dirs :: [FilePath]
    dirs = map libToHie package.libraryDirs ++ [fallback </> packageName]

  firstDir dirs >>= \ case
    Nothing -> do
      unless (null $ package.exposedModules) do
        warning $ "no HIE directory for " <> display (installedComponentId package)
      return []
    Just dir -> do
      hieFiles dir
  where
    warning :: String -> WarningM ()
    warning = tell . return . Warning

    packageName :: String
    packageName = unPackageName . pkgName $ sourcePackageId package

    dependency :: Package
    dependency = Package { type_,  name }
      where
        name :: Text
        name = pack packageName

        type_ :: PackageType
        type_ = case name `Set.member` directDependencies of
          False -> TransitiveDependency
          True -> DirectDependency

    hieFiles :: FilePath -> WarningM [HieFilePath]
    hieFiles dir = catMaybes <$> for package.exposedModules \ case
      ExposedModule name Nothing -> do
        let file = dir </> toFilePath name <.> "hie"
        liftIO (doesFileExist file) >>= \ case
          False -> do
            let isVirtualModule = packageName == "ghc-prim" && name == "GHC.Prim"
            unless isVirtualModule do
              warning $ "non-existing " <> file
            return Nothing
          True -> do
            return . Just $ HieFilePath dependency file
      ExposedModule name (Just (OpenModule _ original)) | name == original -> do
        return Nothing
      ExposedModule name (Just original) -> do
        warning $ "ignoring re-export " <> display original <> " as " <> packageName <> ":" <> display name
        return Nothing

firstDir :: MonadIO m => [FilePath] -> m (Maybe FilePath)
firstDir = liftIO . \ case
  [] -> return Nothing
  dir : dirs -> doesDirectoryExist dir >>= \ case
    True -> return $ Just dir
    False -> firstDir dirs

listPackages :: GHC.Info -> IO [InstalledPackageInfo]
listPackages info = getEnv "GHC_ENVIRONMENT" >>= \ case
  Nothing -> GhcPkg.dump
  Just "-" -> GhcPkg.dump
  Just envFile -> listPackageConfigs envFile info >>= traverse readPackageConfig

readPackageConfig :: FilePath -> IO InstalledPackageInfo
readPackageConfig name = B.readFile name <&> parseInstalledPackageInfo >>= \ case
  Left err -> do
    die . unlines $
        unwords ["Reading", name, "failed!"]
      : ""
      : NonEmpty.toList err
  Right (_, package) -> do
    return package { libraryDirs = map expand package.libraryDirs }
  where
    expand :: FilePath -> FilePath
    expand = unpack . T.replace "${pkgroot}" (pack . takeDirectory $ takeDirectory name) . pack

listPackageConfigs :: FilePath -> GHC.Info -> IO [FilePath]
listPackageConfigs envFile info = do
  entries <- parseEnvironmentFile envFile

  let
    dbs = info.globalPackageDb : [db | PackageDb db <- entries]
    confs = [unpack package <> ".conf" | PackageId package <- entries]

  for confs \ conf -> findFile dbs conf >>= \ case
    Nothing -> die $ unwords ["missing package configuration", conf]
    Just file -> return file

parseEnvironmentFile :: FilePath -> IO [Entry]
parseEnvironmentFile name = T.readFile name <&> parseEntries name >>= either die return

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
  ("package-db": _) -> return (PackageDb (envdir </> unpack db))
    -- relative package dbs are interpreted relative to the env file
    where envdir = takeDirectory envfile
          db = T.drop 11 str
  ["package-id", packageId] -> return $ PackageId packageId
  ["hide-package", package] -> return $ HidePackage package
  [packageId] -> return $ PackageId packageId
  _ -> Left $ "Can't parse environment file entry: " ++ envfile ++ ": " ++ unpack str
