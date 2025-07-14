{-# LANGUAGE CPP #-}
module HIE (
  with
#ifdef TEST
, Result(..)
, readHieFiles
#endif
) where

import Imports

import Data.Double.Conversion.Text qualified as Double
import Data.Map qualified as Map
import Control.Concurrent.Async (Async, withAsync)
import System.IO.Temp (withSystemTempDirectory)

import GHC.Types.Name.Cache

import "ghc-hie" GHC.Iface.Ext.Binary as HIE

import Util
import GHC.Info as GHC (Info(..))
import GHC.EnvironmentFile
import GHC.Diagnostic (Name(..), AvailableImports, ProvidedBy(..))
import GHC.Diagnostic.Annotated

import GHC.HIE

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
  for_ errors $ putStr . pack
  where
    constructing :: IO ()
    constructing = putStr $ "constructing " <> what <> " identifier map... "

    done :: Double -> IO ()
    done dt = do
      if dt == 0 then do
        putStr . pack $ withColor Yellow "⟳\n"
      else do
        putStr $ formatTime dt
        putStr . pack $ withColor Green " ✔\n"

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
    putStrLn message = putStr (pack message <> "\n")

data Result = Result {
  duration :: Double
, errors :: [String]
} deriving (Eq, Show)

instance Semigroup Result where
  a <> b = Result (a.duration + b.duration) (a.errors <> b.errors)

instance Monoid Result where
  mempty = Result 0 []

readHieFiles :: [HieFilePath] -> ((AvailableImports -> AvailableImports) -> IO ()) -> IO Result
readHieFiles files updateAvailableImports = do
  nameCache <- initNameCache 'r' []
  (duration, errors) <- timeAction $ catMaybes <$> for files \ file -> do
    try (readExports nameCache updateAvailableImports file) >>= \ case
      Left err -> return . Just $ formatException file err
      Right () -> return Nothing
  return Result { duration, errors }
  where
    formatException :: HieFilePath -> SomeException -> String
    formatException file err = unlines $
        unwords ["Reading", file.path, "failed:"]
      : map ("  " <>) (lines $ displayWithoutCallStack err)

    displayWithoutCallStack :: SomeException -> String
    displayWithoutCallStack = toException >>> displayException

readExports :: NameCache -> ((AvailableImports -> AvailableImports) -> IO ()) -> HieFilePath -> IO ()
readExports nameCache updateAvailableImports file = do
  result <- readHieFile nameCache file.path

  let
    exports :: [(Name, ProvidedBy)]
    exports = hieExports file.package result.hie_file_result

    insert :: (Name, ProvidedBy) -> AvailableImports -> AvailableImports
    insert (name, providedBy) = Map.insertWith (++) name [providedBy]

    insertAll :: AvailableImports -> AvailableImports
    insertAll m = foldr insert m exports

  updateAvailableImports insertAll

findHieFiles :: FilePath -> IO [HieFilePath]
findHieFiles dir = map (HieFilePath (Package CurrentPackage "main")) <$> listHieFiles dir
