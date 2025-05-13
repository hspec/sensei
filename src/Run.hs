{-# LANGUAGE CPP #-}
module Run (
  run
#ifdef TEST
, RunArgs(..)
, runWith
, defaultRunArgs
, watchFiles
, emitTriggerAndWaitForDelivery
#endif
) where

import qualified Prelude
import           Imports

import qualified Data.ByteString as ByteString
import           System.IO hiding (putStrLn)
import           System.IO.Temp (withSystemTempDirectory)
import qualified System.FSNotify as FSNotify

import qualified HTTP
import qualified Session

import           EventQueue
import           Trigger
import qualified Input
import           Pager (pager)
import           Util
import           Config
import           GHC.Diagnostic

watchFiles :: FilePath -> EventQueue -> IO () -> IO ()
watchFiles dir queue action = do
  FSNotify.withManager $ \ manager -> do
    bracket (FSNotify.watchTree manager dir isInteresting dispatch) (\ stopListening -> stopListening) $ \ _ -> do
      action
  where
    dispatch :: FSNotify.Event -> IO ()
    dispatch = \ case
      FSNotify.Added file _ _ -> emit $ FileEvent FileAdded file
      FSNotify.Modified file _ _ -> emit $ FileEvent FileModified file
      FSNotify.ModifiedAttributes file _ _ -> emit $ FileEvent FileModified file
      FSNotify.Removed file _ _ -> emit $ FileEvent FileRemoved file
      FSNotify.WatchedDirectoryRemoved _file _ _ -> pass
      FSNotify.CloseWrite file _ _ -> emit $ FileEvent FileModified file
      FSNotify.Unknown file _ _ _ -> emit $ FileEvent FileModified file

    emit :: EventQueue.Event -> IO ()
    emit = emitEvent queue

    isInteresting :: FSNotify.Event -> Bool
    isInteresting = (&&) <$> isFile <*> not . isBoring . FSNotify.eventPath

    isFile :: FSNotify.Event -> Bool
    isFile = FSNotify.eventIsDirectory >>> (== FSNotify.IsFile)

data Mode = Lenient | Strict

run :: [String] -> IO ()
run args = withSystemTempDirectory "sensei" \ hieDir -> do
  config <- loadConfig
  runArgs@RunArgs{dir, lastOutput, queue, cleanupAction} <- defaultRunArgs (Just hieDir)

  let
    putStrLn :: String -> IO ()
    putStrLn message = do
      cleanupAction.add $ Prelude.putStrLn message
      cleanupAction.run

    appConfig :: HTTP.AppConfig
    appConfig = HTTP.AppConfig {
      dir
    , hieDir
    , putStrLn
    , deepSeek = config.deepSeek
    , trigger = emitTriggerAndWaitForDelivery queue
    , getLastResult = readMVar lastOutput
    , getModules = readIORef runArgs.modules
    }

    watch :: IO () -> IO ()
    watch
      | config.watch = watchFiles dir queue
      | otherwise = id

  HTTP.withApp appConfig do
    watch do
      mode <- newIORef Lenient
      Input.watch stdin (dispatch mode queue) (emitEvent queue Done)
      runWith runArgs {config, args}
  where
    dispatch :: IORef Mode -> EventQueue -> Char -> IO ()
    dispatch mode queue = \ case
      '\n' -> emitEvent queue TriggerAll
      'w' -> do
        modifyIORef mode toggle
        readIORef mode >>= \ case
          Lenient -> emitEvent queue (RestartWith ["-Wdefault"])
          Strict -> emitEvent queue (RestartWith ["-Wall"])
      'q' -> emitEvent queue Done
      _ -> pass

    toggle :: Mode -> Mode
    toggle = \ case
      Lenient -> Strict
      Strict -> Lenient

emitTriggerAndWaitForDelivery :: EventQueue -> IO ()
emitTriggerAndWaitForDelivery queue = do
  barrier <- newEmptyMVar
  emitEvent queue . Trigger $ OnTestRunStarted do
    putMVar barrier ()
  takeMVar barrier

defaultRunArgs :: Maybe FilePath -> IO RunArgs
defaultRunArgs hieDir = do
  queue <- newQueue
  lastOutput <- newMVar (Trigger.Success, "", [])
  modules <- newIORef []
  cleanupAction <- newCleanupAction
  return RunArgs {
    config = defaultConfig
  , dir = ""
  , args = []
  , lastOutput
  , modules
  , queue = queue
  , sessionConfig = Session.Config {
      configIgnoreDotGhci = False
    , configIgnore_GHC_ENVIRONMENT = False
    , configWorkingDirectory = Nothing
    , configHieDirectory = hieDir
    , configEcho = \ string -> ByteString.putStr string >> hFlush stdout
    }
  , withSession = Session.withSession
  , cleanupAction
  }

data RunArgs = RunArgs {
  config :: Config
, dir :: FilePath
, args :: [String]
, lastOutput :: MVar (Result, String, [Annotated])
, modules :: IORef [String]
, queue :: EventQueue
, sessionConfig  :: Session.Config
, withSession :: forall r. Session.Config -> [String] -> (Session.Session -> IO r) -> IO r
, cleanupAction :: CleanupAction
}

data CleanupAction = CleanupAction {
  run :: IO ()
, add :: IO () -> IO ()
}

newCleanupAction :: IO CleanupAction
newCleanupAction = do
  mvar <- newMVar pass
  return CleanupAction {
    run = modifyMVar_ mvar \ action -> do
      action
      return pass
  , add = \ new -> modifyMVar_ mvar \ existing -> do
      return $ existing >> new
  }

runWith :: RunArgs -> IO ()
runWith RunArgs {..} = do
  let
    saveOutput :: IO (Trigger.Result, String, [Annotated]) -> OnTestRunStarted -> IO ()
    saveOutput action onTestRunStarted = do
      cleanupAction.run
      result <- modifyMVar lastOutput $ \ _ -> do
        onTestRunStarted.notify
        (id &&& id) <$> action
      case result of
        (HookFailed, _output, _diagnostics) -> pass
        (Failure, output, _diagnostics) -> config.senseiHooksOnFailure >>= \ case
          HookSuccess -> pager output >>= cleanupAction.add
          HookFailure message -> hPutStrLn stderr message
        (Success, _output, _diagnostics) -> config.senseiHooksOnSuccess >>= \ case
          HookSuccess -> pass
          HookFailure message -> hPutStrLn stderr message

    hooks :: Hooks
    hooks = Hooks {
      beforeReload = config.senseiHooksBeforeReload
    , afterReload = config.senseiHooksAfterReload
    }

    go :: OnTestRunStarted -> [String] -> IO ()
    go onTestRunStarted extraArgs = do
      status <- withSession sessionConfig (extraArgs <> args) $ \ session -> do
        let
          printExtraArgs :: IO ()
          printExtraArgs = forM_ extraArgs $ \ arg -> do
            sessionConfig.configEcho . encodeUtf8 . withColor Red $ arg <> "\n"

          triggerAction :: OnTestRunStarted -> IO ()
          triggerAction = saveOutput (trigger modules session hooks <* printExtraArgs)

          triggerAllAction :: OnTestRunStarted -> IO ()
          triggerAllAction = saveOutput (triggerAll modules session hooks <* printExtraArgs)

        triggerAction onTestRunStarted
        processQueue cleanupAction.run (sessionConfig.configEcho . encodeUtf8) dir queue triggerAllAction triggerAction
      case status of
        (notify, Restart mExtraArgs) -> go notify (fromMaybe extraArgs mExtraArgs)
        (OnTestRunStarted notify, Terminate) -> notify
  go mempty []
