{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
module EventQueue (
  EventQueue
, newQueue

, Event(..)
, FileEventType(..)
, emitEvent

, Status(..)
, processQueue

#ifdef TEST
, Action(..)
, processEvents
, combineFileEvents
, groupFileEvents
#endif
) where

import           Imports
import qualified Data.Set as Set

import           Control.Monad.STM
import           Control.Concurrent.STM.TChan

import           Util

type EventQueue = TChan Event

data Event =
    TriggerAll
  | FileEvent FileEventType FilePath
  | RestartWith [String]
  | Done
  deriving (Eq, Show)

data FileEventType = FileAdded | FileRemoved | FileModified
  deriving (Eq, Show)

newQueue :: IO EventQueue
newQueue = atomically newTChan

emitEvent :: EventQueue -> Event -> IO ()
emitEvent chan = atomically . writeTChan chan

readEvents :: EventQueue -> IO [Event]
readEvents chan = do
  e <- atomically $ readTChan chan
  unless (isKeyboardInput e) $ do
    threadDelay 100_000
  es <- atomically emptyQueue
  return (e : es)
  where
    isKeyboardInput :: Event -> Bool
    isKeyboardInput = \ case
      TriggerAll -> True
      FileEvent {} -> False
      RestartWith {} -> True
      Done -> True

    emptyQueue :: STM [Event]
    emptyQueue = do
      mEvent <- tryReadTChan chan
      case mEvent of
        Nothing -> return []
        Just e -> (e :) <$> emptyQueue

data Status = Terminate | Restart (Maybe [String])
  deriving (Eq, Show)

processQueue :: IO () -> (String -> IO ()) -> FilePath -> EventQueue -> IO () -> IO () -> IO Status
processQueue cleanup echo dir chan triggerAll trigger = go
  where
    go :: IO Status
    go = do
      action <- readEvents chan >>= processEvents echo dir
      runCleanup action
      case action of
        NoneAction -> do
          go
        TriggerAction files -> do
          output files
          trigger
          go
        TriggerAllAction -> do
          triggerAll
          go
        RestartAction file t -> do
          output [file <> " (" <> show t <> ", restarting)"]
          return $ Restart Nothing
        RestartWithAction args -> do
          return $ Restart (Just args)
        DoneAction -> do
          return Terminate

    runCleanup :: Action -> IO ()
    runCleanup = \ case
      NoneAction -> pass
      TriggerAction {} -> cleanup
      TriggerAllAction -> cleanup
      RestartAction {} -> cleanup
      RestartWithAction {} -> cleanup
      DoneAction -> cleanup

    output :: [String] -> IO ()
    output = mapM_ (\ name -> echo . withInfoColor $ "--> " <> name <> "\n")

data Action =
    NoneAction
  | TriggerAction [FilePath]
  | TriggerAllAction
  | RestartAction FilePath FileEventType
  | RestartWithAction [String]
  | DoneAction
  deriving (Eq, Show)

processEvents :: (String -> IO ()) -> FilePath -> [Event] -> IO Action
processEvents echo dir events = do
  files <- fileEvents echo dir events
  return $ if
    | Done `elem` events -> DoneAction
    | args : _ <- [args | RestartWith args <- reverse events] -> RestartWithAction args
    | (file, t) : _ <- filter shouldRestart files -> RestartAction file t
    | TriggerAll `elem` events -> TriggerAllAction
    | not (null files) -> TriggerAction . Set.toList . Set.fromList $ map fst files
    | otherwise -> NoneAction

shouldRestart :: (FilePath, FileEventType) -> Bool
shouldRestart = (||) <$> specAddedOrRemoved <*> dotGhciModified
  where
    specAddedOrRemoved :: (FilePath, FileEventType) -> Bool
    specAddedOrRemoved (name, event) = "Spec.hs" `isSuffixOf` name && case event of
      FileAdded -> True
      FileRemoved -> True
      FileModified -> False

    dotGhciModified :: (FilePath, FileEventType) -> Bool
    dotGhciModified (name, _) = takeFileName name == ".ghci"

fileEvents :: (String -> IO ()) -> FilePath -> [Event] -> IO [(FilePath, FileEventType)]
fileEvents echo dir events = filterGitIgnored echo dir $ combineFileEvents [(p, e) | FileEvent e p <- events]

filterGitIgnored :: (String -> IO ()) -> FilePath -> [(FilePath, FileEventType)] -> IO [(FilePath, FileEventType)]
filterGitIgnored echo dir events = map f <$> filterGitIgnoredFiles echo dir (map fst events)
  where
    f :: FilePath -> (FilePath, FileEventType)
    f p = (p, fromJust $ lookup p events)

combineFileEvents :: [(FilePath, FileEventType)] -> [(FilePath, FileEventType)]
combineFileEvents events = [(file, e) | (file, Just e) <- map (second combineFileEventTypes) $ groupFileEvents events]

groupFileEvents :: [(FilePath, FileEventType)] -> [(FilePath, [FileEventType])]
groupFileEvents = map (second $ map snd) . groupOn fst

groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn f = go
  where
    go = \ case
      [] -> []
      x : xs -> case partition (\ a -> f a == f x) xs of
        (ys, zs) -> (f x, (x : ys)) : go zs

combineFileEventTypes :: [FileEventType] -> Maybe FileEventType
combineFileEventTypes = go
  where
    go events = case events of
      [] -> Nothing
      [e] -> Just e
      e1 : e2 : es -> go $ (combine e1 e2) es

    combine e1 e2 = case (e1, e2) of
      (FileAdded, FileAdded) -> ignoreDuplicate FileAdded
      (FileAdded, FileRemoved) -> id
      (FileAdded, FileModified) -> (FileAdded :)

      (FileRemoved, FileAdded) -> (FileModified :)
      (FileRemoved, FileRemoved) -> ignoreDuplicate FileRemoved
      (FileRemoved, FileModified) -> shouldNeverHappen

      (FileModified, FileAdded) -> shouldNeverHappen
      (FileModified, FileRemoved) -> (FileRemoved :)
      (FileModified, FileModified) -> ignoreDuplicate FileModified

    ignoreDuplicate = (:)
    shouldNeverHappen = (FileModified :)
