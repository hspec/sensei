{-# LANGUAGE CPP #-}
module HIE (
  with
#ifdef TEST
, LoadResult(..)
, loadPackages
#endif
) where

import Imports

import Data.Coerce
import Data.Text qualified as T
import Data.Double.Conversion.Text qualified as Double
import Data.Map qualified as Map
import Control.Concurrent.Async (Async, withAsync)
import System.Process
import System.IO.Temp (withSystemTempDirectory)

import GHC.Unit.Types qualified
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Name.Cache
import Language.Haskell.Syntax.Module.Name

import Data.Text.Internal (text)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import GHC.Data.FastString (FastString, fs_sbs)

import "ghc-hie" GHC.Iface.Ext.Types as HIE
import "ghc-hie" GHC.Iface.Ext.Binary as HIE

import Util
import GHC.Info as GHC (Info(..))
import GHC.EnvironmentFile
import GHC.Diagnostic (AvailableImports)
import GHC.Diagnostic.Annotated (Module(..))

type PutStr = Text -> IO ()

with :: PutStr -> GHC.Info -> (FilePath -> IO AvailableImports -> Async () -> IO a) -> IO a
with putStr info action = withSystemTempDirectory "sensei" \ hieDir -> do
  globalIdentifierMap <- newIORef mempty
  globalLoadResult <- newIORef mempty

  let
    getAvailableImports :: IO AvailableImports
    getAvailableImports = do
      reportConstructionTime putStr "global" do
        atomicReadIORef globalLoadResult

      identifierMap <- atomicReadIORef globalIdentifierMap >>= newIORef
      reportConstructionTime putStr "local" do
        (dt, files) <- timeAction $ findHieFiles hieDir
        (LoadResult dt [] <>) <$> loadPackages files (modifyIORef identifierMap)
      readIORef identifierMap

  asyncLoadAllPackages putStr info globalIdentifierMap globalLoadResult do
    action hieDir getAvailableImports

reportConstructionTime :: PutStr -> Text -> IO LoadResult -> IO ()
reportConstructionTime putStr what action = do
  constructing
  LoadResult duration errors <- action
  done duration
  for_ errors $ putStr . T.pack
  where
    constructing :: IO ()
    constructing = putStr $ "constructing " <> what <> " identifier map... "

    done :: Double -> IO ()
    done dt = do
      if dt == 0 then do
        putStr . T.pack $ withColor Yellow "⟳\n"
      else do
        putStr $ formatTime dt
        putStr . T.pack $ withColor Green " ✔\n"

    formatTime :: Double -> Text
    formatTime dt = Double.toFixed 2 dt <> "s"

asyncLoadAllPackages :: PutStr -> GHC.Info -> IORef AvailableImports -> IORef LoadResult -> (Async () -> IO a) -> IO a
asyncLoadAllPackages putStr info identifierMap loadResult action = do
  (warnings, files) <- listAllHieFiles info
  for_ warnings \ (Warning message) -> do
    putStrLn $ withColor Red message
  let
    load :: IO ()
    load = do
      loadPackages files \ insertAll -> do
        atomicModifyIORef' identifierMap (insertAll &&& pass)
      >>= writeIORef loadResult
  withAsync load action
  where
    putStrLn :: String -> IO ()
    putStrLn message = putStr (T.pack message <> "\n")

data LoadResult = LoadResult {
  duration :: Double
, errors :: [String]
} deriving (Eq, Show)

instance Semigroup LoadResult where
  a <> b = LoadResult (a.duration + b.duration) (a.errors <> b.errors)

instance Monoid LoadResult where
  mempty = LoadResult 0 []

loadPackages :: [FilePath] -> ((AvailableImports -> AvailableImports) -> IO ()) -> IO LoadResult
loadPackages files updateAvailableImports = do
  nameCache <- initNameCache 'r' []
  (duration, errors) <- timeAction $ catMaybes <$> for files \ file -> do
    try (readExports nameCache updateAvailableImports file) >>= \ case
      Left err -> return . Just $ formatException file err
      Right () -> return Nothing
  return LoadResult { duration, errors }
  where
    formatException :: FilePath -> SomeException -> String
    formatException file err = unlines $
        unwords ["Reading", file, "failed:"]
      : map ("  " <>) (lines $ displayWithoutCallStack err)

    displayWithoutCallStack :: SomeException -> String
    displayWithoutCallStack = toException >>> displayException

readExports :: NameCache -> ((AvailableImports -> AvailableImports) -> IO ()) -> FilePath -> IO ()
readExports nameCache updateAvailableImports file = do
  result <- readHieFile nameCache file

  let
    (module_, names) = hieExports result.hie_file_result

    insert :: Text -> AvailableImports -> AvailableImports
    insert name = Map.insertWith (++) name [module_]

    insertAll :: AvailableImports -> AvailableImports
    insertAll m = foldr insert m names

  updateAvailableImports insertAll

hieExports :: HieFile -> (Module, [Text])
hieExports hieFile = (fromModuleName hieFile.hie_module.moduleName, map fromName exports)
  where
    exports :: [Name]
    exports = concatMap allNames hieFile.hie_exports

    fromName :: Name -> Text
    fromName = unpackFS . occNameFS . nameOccName

    fromModuleName :: ModuleName -> Module
    fromModuleName = Module . unpackFS . coerce

    allNames :: AvailInfo -> [Name]
    allNames = \ case
      Avail name -> [name]
      AvailTC _ names -> names

unpackFS :: FastString -> Text
unpackFS = unsafeShortByteStringAsText . fs_sbs

unsafeShortByteStringAsText :: ShortByteString -> Text
unsafeShortByteStringAsText bs = text bs.unShortByteString 0 (ShortByteString.length bs)

findHieFiles :: FilePath -> IO [FilePath]
findHieFiles dir = lines <$> readCreateProcess (shell $ "find " <> dir <> " -name '*.hie'") ""

-- TODO:
-- * findHieFiles -- don't use `find`
