module HIE (with) where

import System.Process
import Util
import System.IO.Temp (withSystemTempDirectory)
import Data.Double.Conversion.Text qualified as Double
import Control.Concurrent.Async (withAsync)
import           GHC.Types.Name
import           Language.Haskell.Syntax.Module.Name
import           Data.Coerce
import           GHC.Data.FastString
import Data.Map qualified as Map
import           Prelude ()
import           Imports

import           GHC.Diagnostic
import           GHC.Diagnostic.Annotated

import qualified Data.Text as T

import "ghc-hie" GHC.Iface.Ext.Binary as HIE
import "ghc-hie" GHC.Iface.Ext.Types as HIE
import "ghc" GHC.Types.Name.Cache
import "ghc" GHC.Types.Avail
import qualified GHC.Unit.Types as GHC

import GHC.EnvironmentFile (listAllHieFiles)

hieExports :: HieFileResult -> (Module, [Text])
hieExports r = (Module . T.pack . unpackFS $ coerce hieFile.hie_module.moduleName, foo)
  where
    hieFile = r.hie_file_result

    foo :: [Text]
    foo = map (T.pack . occNameString . nameOccName) $ concatMap bar hieFile.hie_exports

    bar :: AvailInfo -> [Name]
    bar = \ case
      Avail name -> [name]
      AvailTC name names -> name : names

loadAllPackages :: [FilePath] -> IORef AvailableImports -> IO ()
loadAllPackages files mvar = do
  nameCache <- initNameCache 'r' []
  for_ files \ file -> do
    readExports nameCache mvar file

readExports :: NameCache -> IORef AvailableImports -> FilePath -> IO ()
readExports nameCache mvar file = do
    r <- readHieFile nameCache file

    let
      (mod, y) = hieExports r

    let
      insert :: Text -> AvailableImports -> AvailableImports
      insert name = Map.insertWith (++) name [mod]

      insertAll :: AvailableImports -> AvailableImports
      insertAll m = foldr insert m y

    -- modifyIORef mvar insertAll
    atomicModifyIORef' mvar (insertAll &&& const ())

asyncLoadAllPackages :: IORef AvailableImports -> IORef Double -> IO b -> IO b
asyncLoadAllPackages identifierMap identifierMapConstructionTime action = do
  files <- listAllHieFiles
  let
    load = do
      ((), dt) <- timeAction do
        HIE.loadAllPackages files identifierMap
      atomicWriteIORef identifierMapConstructionTime dt
  withAsync load \ _ -> action

with :: (FilePath -> IO AvailableImports -> IO a) -> IO a
with action = withSystemTempDirectory "sensei" \ hieDir -> do
  globalIdentifierMap <- newIORef mempty
  globalIdentifierMapConstructionTime <- newIORef 0

  let
    -- getAvailableImports :: Maybe String -> IORef AvailableImports -> IORef Double -> IO AvailableImports
    getAvailableImports = do
      do
        dt <- liftIO $ atomicReadIORef globalIdentifierMapConstructionTime
        if dt == 0 then do
          putStr $ withColor Yellow "constructing global identifier map..." <> "\n"
        else do
          let
            t = Double.toFixed 2 dt
          putStr $ "constructing global identifier map done in " <> T.unpack t <> "s"
          putStr $ withColor Green " ✔\n"

      putStr $ "constructing local identifier map... "
      (r, dt) <- timeAction do
        c <- atomicReadIORef globalIdentifierMap
        ref <- newIORef c
        files <- findHieFiles hieDir
        HIE.loadAllPackages files ref
        readIORef ref
      let
        t = Double.toFixed 2 dt
      putStr $ "" <> T.unpack t <> "s" -- fixme, is putStr ok here?
      putStr $ withColor Green " ✔\n"
      return r

  HIE.asyncLoadAllPackages globalIdentifierMap globalIdentifierMapConstructionTime do
    action hieDir getAvailableImports

findHieFiles :: FilePath -> IO [FilePath]
findHieFiles dir = lines <$> readCreateProcess (shell $ "find " <> dir <> " -name '*.hie'") ""
