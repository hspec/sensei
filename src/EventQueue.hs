{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
module EventQueue (
  EventQueue
, newQueue

, Event(..)
, OnTestRunStarted(..)
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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import           Control.Monad.STM
import           Control.Concurrent.STM.TChan

import           Util

type EventQueue = TChan Event

data Event =
    Trigger OnTestRunStarted
  | TriggerAll
  | FileEvent FileEventType FilePath
  | RestartWith [String]
  | Done

newtype OnTestRunStarted = OnTestRunStarted { notify :: IO () }
  deriving (Semigroup, Monoid)

data FileEventType = FileAdded | FileRemoved | FileModified
  deriving (Eq, Show)

newQueue :: IO EventQueue
newQueue = atomically newTChan

emitEvent :: EventQueue -> Event -> IO ()
emitEvent chan = atomically . writeTChan chan

readEvents :: EventQueue -> IO (NonEmpty Event)
readEvents chan = do
  e <- atomically $ readTChan chan
  unless (isUserInput e) $ do
    threadDelay 100_000
  es <- atomically emptyQueue
  return (e :| es)
  where
    isUserInput :: Event -> Bool
    isUserInput = \ case
      Trigger {} -> True
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

processQueue :: IO () -> (String -> IO ()) -> FilePath -> EventQueue -> (OnTestRunStarted -> IO ()) -> (OnTestRunStarted -> IO ()) -> IO (OnTestRunStarted, Status)
processQueue cleanup echo dir chan triggerAll trigger = go
  where
    nextEvent :: IO (OnTestRunStarted, Action)
    nextEvent = readEvents chan >>= processEvents echo dir >>= \ case
      (notify, Just action) -> return (notify, action)
      (notify, Nothing) -> first (notify <>) <$> nextEvent

    go :: IO (OnTestRunStarted, Status)
    go = do
      event <- nextEvent
      cleanup
      case event of
        (notify, TriggerAction files) -> do
          output files
          trigger notify
          go
        (notify, TriggerAllAction) -> do
          triggerAll notify
          go
        (notify, RestartAction file t) -> do
          output [file <> " (" <> show t <> ", restarting)"]
          return $ (notify, Restart Nothing)
        (notify, RestartWithAction args) -> do
          return $ (notify, Restart (Just args))
        (notify, DoneAction) -> do
          return (notify, Terminate)

    output :: [String] -> IO ()
    output = mapM_ (\ name -> echo . withInfoColor $ "--> " <> name <> "\n")

data Action =
    TriggerAction [FilePath]
  | TriggerAllAction
  | RestartAction FilePath FileEventType
  | RestartWithAction [String]
  | DoneAction
  deriving (Eq, Show)

processEvents :: (String -> IO ()) -> FilePath -> NonEmpty Event -> IO (OnTestRunStarted, Maybe Action)
processEvents echo dir (NonEmpty.toList -> events) = do
  files <- fileEvents echo dir events
  let
    triggerActions :: [OnTestRunStarted]
    triggerActions = [notify | Trigger notify <- events]

    onTestRunStarted :: OnTestRunStarted
    onTestRunStarted = mconcat triggerActions

  return . (,) onTestRunStarted $ if
    | or [True | Done <- events] -> Just DoneAction
    | args : _ <- [args | RestartWith args <- reverse events] -> Just $ RestartWithAction args
    | (file, t) : _ <- filter shouldRestart files -> Just $ RestartAction file t
    | or [True | TriggerAll <- events] -> Just TriggerAllAction
    | (not . null) triggerActions -> Just $ TriggerAction []
    | not (null files) -> Just . TriggerAction . Set.toList . Set.fromList $ map fst files
    | otherwise -> Nothing

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
