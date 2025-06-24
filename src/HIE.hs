{-# LANGUAGE CPP #-}
module HIE (
  with
#ifdef TEST
, Result(..)
, readHieFiles
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
import GHC.Diagnostic (AvailableImports, ProvidedBy(..))
import GHC.Diagnostic.Annotated (Module(..), Type(..))

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

      ref <- atomicReadIORef globalIdentifierMap >>= newIORef
      reportConstructionTime putStr "local" do
        (dt, files) <- timeAction $ findHieFiles hieDir
        (Result dt [] <>) <$> readHieFiles files (modifyIORef' ref)
      readIORef ref

  asyncLoadAllPackages putStr info globalIdentifierMap globalLoadResult do
    action hieDir getAvailableImports

reportConstructionTime :: PutStr -> Text -> IO Result -> IO ()
reportConstructionTime putStr what action = do
  constructing
  Result duration errors <- action
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

asyncLoadAllPackages :: PutStr -> GHC.Info -> IORef AvailableImports -> IORef Result -> (Async () -> IO a) -> IO a
asyncLoadAllPackages putStr info identifierMap loadResult action = do
  (warnings, files) <- listAllHieFiles info
  for_ warnings \ (Warning message) -> do
    putStrLn $ withColor Red message
  let
    load :: IO ()
    load = do
      readHieFiles files \ insertAll -> do
        atomicModifyIORef' identifierMap (insertAll &&& mempty)
      >>= writeIORef loadResult
  withAsync load action
  where
    putStrLn :: String -> IO ()
    putStrLn message = putStr (T.pack message <> "\n")

data Result = Result {
  duration :: Double
, errors :: [String]
} deriving (Eq, Show)

instance Semigroup Result where
  a <> b = Result (a.duration + b.duration) (a.errors <> b.errors)

instance Monoid Result where
  mempty = Result 0 []

readHieFiles :: [Path HieFile] -> ((AvailableImports -> AvailableImports) -> IO ()) -> IO Result
readHieFiles files updateAvailableImports = do
  nameCache <- initNameCache 'r' []
  (duration, errors) <- timeAction $ catMaybes <$> for files \ file -> do
    try (readExports nameCache updateAvailableImports file) >>= \ case
      Left err -> return . Just $ formatException file err
      Right () -> return Nothing
  return Result { duration, errors }
  where
    formatException :: Path HieFile -> SomeException -> String
    formatException (Path file) err = unlines $
        unwords ["Reading", file, "failed:"]
      : map ("  " <>) (lines $ displayWithoutCallStack err)

    displayWithoutCallStack :: SomeException -> String
    displayWithoutCallStack = toException >>> displayException

readExports :: NameCache -> ((AvailableImports -> AvailableImports) -> IO ()) -> Path HieFile -> IO ()
readExports nameCache updateAvailableImports (Path file) = do
  result <- readHieFile nameCache file

  let
    exports :: [(Text, ProvidedBy)]
    exports = hieExports result.hie_file_result

    insert :: (Text, ProvidedBy) -> AvailableImports -> AvailableImports
    insert (name, providedBy) = Map.insertWith (++) name [providedBy]

    insertAll :: AvailableImports -> AvailableImports
    insertAll m = foldr insert m exports

  updateAvailableImports insertAll

hieExports :: HieFile -> [(Text, ProvidedBy)]
hieExports hieFile = concatMap allNames hieFile.hie_exports
  where
    module_ :: Module
    module_ = fromModuleName hieFile.hie_module.moduleName

    fromName :: Name -> Text
    fromName = unpackFS . occNameFS . nameOccName

    fromModuleName :: ModuleName -> Module
    fromModuleName = Module . unpackFS . coerce

    allNames :: AvailInfo -> [(Text, ProvidedBy)]
    allNames = \ case
      Avail name -> [(fromName name, ProvidedBy module_ Nothing)]
      AvailTC type_ names -> [(name, ProvidedBy module_ (Just . Type $ fromName type_)) | name <- map fromName names]

unpackFS :: FastString -> Text
unpackFS = unsafeShortByteStringAsText . fs_sbs

unsafeShortByteStringAsText :: ShortByteString -> Text
unsafeShortByteStringAsText bs = text bs.unShortByteString 0 (ShortByteString.length bs)

findHieFiles :: FilePath -> IO [Path HieFile]
findHieFiles dir = map Path . lines <$> readCreateProcess (shell $ "find " <> dir <> " -name '*.hie'") ""
