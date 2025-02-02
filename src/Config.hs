{-# LANGUAGE CPP #-}
module Config (
  Config(..)
, Hook
, HookResult(..)
, defaultConfig
, loadConfig
#ifdef TEST
, ConfigFile(..)
, readConfigFilesFrom
, tryReadFile
#endif
) where

import Imports

import Data.ByteString qualified as ByteString
import System.Directory
import System.Process
import Data.Aeson
import Data.Yaml

import Util
import Config.DeepSeek

configFilename :: FilePath
configFilename = "sensei.yaml"

data ConfigFile = ConfigFile {
  -- | Shell command to run after a file has changed, but before reloading. Cancels the current
  -- trigger cycle when exiting with non-zero exit code.
  beforeReload :: Maybe String
  -- | Shell command to run after a successful reload, but before running the tests. Cancels the
  -- current trigger cycle when exiting with non-zero exit code.
, afterReload :: Maybe String
  -- | Shell command to run after the trigger cycle completed with failure. Has access to the result
  -- via seito.
, onFailure :: Maybe String
  -- | Shell command to run after the trigger cycle successfully completed. Has access to the result
  -- via seito.
, onSuccess :: Maybe String
, deepSeek :: Maybe DeepSeek
} deriving (Eq, Show, Generic)

instance Semigroup ConfigFile where
  a <> b = ConfigFile {
    beforeReload = a.beforeReload <|> b.beforeReload
  , afterReload = a.afterReload <|> b.afterReload
  , onFailure = a.onFailure <|> b.onFailure
  , onSuccess = a.onSuccess <|> b.onSuccess
  , deepSeek = a.deepSeek <|> b.deepSeek
  }

instance Monoid ConfigFile where
  mempty = ConfigFile Nothing Nothing Nothing Nothing Nothing

instance FromJSON ConfigFile where
  parseJSON = \ case
    Null -> return mempty
    value -> genericKebabDecode value

type Hook = IO HookResult

data HookResult = HookSuccess | HookFailure String

data Config = Config {
  senseiHooksBeforeReload :: Hook
, senseiHooksAfterReload :: Hook
, senseiHooksOnSuccess :: Hook
, senseiHooksOnFailure :: Hook
, deepSeek :: Maybe DeepSeek
}

tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile = fmap (either (const Nothing) Just) . tryJust (guard . isDoesNotExistError) . ByteString.readFile

readConfigFile :: FilePath -> IO (Either String ConfigFile)
readConfigFile path = tryReadFile path >>= \ case
  Just input -> return $ first displayException . decodeEither' $ input
  Nothing -> return $ Right mempty

readConfigFiles :: IO (Either String ConfigFile)
readConfigFiles = do
  configDir <- getXdgDirectory XdgConfig "sensei"
  readConfigFilesFrom (configDir </> configFilename) configFilename

readConfigFilesFrom :: FilePath -> FilePath -> IO (Either String ConfigFile)
readConfigFilesFrom global local = fmap mconcat . sequence <$> sequence [
    readConfigFile local
  , readConfigFile global
  ]

toConfig :: ConfigFile -> Config
toConfig ConfigFile{..} = Config {
  senseiHooksBeforeReload = maybeToHook "before-reload" beforeReload
, senseiHooksAfterReload = maybeToHook "after-reload" afterReload
, senseiHooksOnSuccess = maybeToHook "on-success" onSuccess
, senseiHooksOnFailure = maybeToHook "on-failure" onFailure
, deepSeek
} where
    maybeToHook :: String -> Maybe String -> Hook
    maybeToHook name = maybe (return HookSuccess) (toHook name)

    toHook :: String -> String -> Hook
    toHook name command = system command >>= \ case
      ExitSuccess -> return HookSuccess
      ExitFailure code -> do
        let message = withColor Red $ name <> ": " <> command <> "  # failed with status " <> show code
        return $ HookFailure message

defaultConfig :: Config
defaultConfig = toConfig mempty

loadConfig :: IO Config
loadConfig = readConfigFiles >>= either (throwIO . ErrorCall) (return . toConfig)
