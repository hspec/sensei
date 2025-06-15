{-# LANGUAGE CPP #-}
module HIE (
  with
#ifdef TEST
, loadAllPackages
#endif
) where

import Imports

import Data.Coerce
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import Data.Double.Conversion.Text qualified as Double
import Data.Map qualified as Map
import Control.Concurrent.Async (withAsync)
import System.Process
import System.IO.Temp (withSystemTempDirectory)

import GHC.Unit.Types qualified
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Data.FastString
import Language.Haskell.Syntax.Module.Name

import "ghc-hie" GHC.Iface.Ext.Types as HIE
import "ghc-hie" GHC.Iface.Ext.Binary as HIE

import Util
import GHC.Info as GHC (Info(..))
import GHC.EnvironmentFile (listAllHieFiles)
import GHC.Diagnostic (AvailableImports)
import GHC.Diagnostic.Annotated (Module(..))

with :: GHC.Info -> (FilePath -> IO AvailableImports -> IO a) -> IO a
with info action = withSystemTempDirectory "sensei" \ hieDir -> do
  globalIdentifierMap <- newIORef mempty
  globalIdentifierMapConstructionTime <- newIORef 0

  let
    getAvailableImports :: IO AvailableImports
    getAvailableImports = do
      reportConstructionTime "global" do
        atomicReadIORef globalIdentifierMapConstructionTime

      ref <- atomicReadIORef globalIdentifierMap >>= newIORef
      reportConstructionTime "local" $ timeAction_ do
        files <- findHieFiles hieDir
        loadAllPackages files (modifyIORef ref)
      readIORef ref

  asyncLoadAllPackages info globalIdentifierMap globalIdentifierMapConstructionTime do
    action hieDir getAvailableImports

reportConstructionTime :: Text -> IO Double -> IO ()
reportConstructionTime what action = constructing >> action >>= done
  where
    constructing :: IO ()
    constructing = T.putStr $ "constructing " <> what <> " identifier map... "

    done :: Double -> IO ()
    done dt = do
      if dt == 0 then do
        T.putStr . T.pack $ withColor Yellow "⟳\n"
      else do
        T.putStr $ formatTime dt
        T.putStr . T.pack $ withColor Green " ✔\n"

    formatTime :: Double -> Text
    formatTime dt = Double.toFixed 2 dt <> "s"

asyncLoadAllPackages :: GHC.Info -> IORef AvailableImports -> IORef Double -> IO b -> IO b
asyncLoadAllPackages info identifierMap identifierMapConstructionTime action = do
  files <- listAllHieFiles info
  let
    load = do
      dt <- timeAction_ do
        loadAllPackages files \ insertAll -> do
          atomicModifyIORef' identifierMap (insertAll &&& const ())
      atomicWriteIORef identifierMapConstructionTime dt
  withAsync load \ _ -> action

loadAllPackages :: [FilePath] -> ((AvailableImports -> AvailableImports) -> IO a) -> IO ()
loadAllPackages files mvar = do
  nameCache <- initNameCache 'r' []
  for_ files $ readExports nameCache mvar

readExports :: NameCache -> ((AvailableImports -> AvailableImports) -> IO a) -> FilePath -> IO a
readExports nameCache updateAvailableImports file = do
  result <- readHieFile nameCache file

  let
    (module_, names) = hieExports result

    insert :: Text -> AvailableImports -> AvailableImports
    insert name = Map.insertWith (++) name [module_]

    insertAll :: AvailableImports -> AvailableImports
    insertAll m = foldr insert m names

  updateAvailableImports insertAll

hieExports :: HieFileResult -> (Module, [Text])
hieExports result = (fromModuleName hieFile.hie_module.moduleName, map fromName exports)
  where
    hieFile :: HieFile
    hieFile = result.hie_file_result

    exports :: [Name]
    exports = concatMap allNames result.hie_file_result.hie_exports

    fromName :: Name -> Text
    fromName = T.pack . occNameString . nameOccName

    fromModuleName :: ModuleName -> Module
    fromModuleName = Module . T.pack . unpackFS . coerce

    allNames :: AvailInfo -> [Name]
    allNames = \ case
      Avail name -> [name]
      AvailTC name names -> name : names

findHieFiles :: FilePath -> IO [FilePath]
findHieFiles dir = lines <$> readCreateProcess (shell $ "find " <> dir <> " -name '*.hie'") ""

-- TODO:
-- * findHieFiles -- don't use `find`
