module GHC.Diagnostic.Edit (
  cut
, replaceFirst
) where

import Data.Text.Array (Array)
import Imports

import Data.Text qualified as T
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Search qualified as Search

import GHC.Diagnostic.Type (Location(..))

cut :: Location -> Location -> Text -> (Text, Text, Text)
cut start end input@(Text arr startOfInput len) = (
    slice startOfInput startOffset
  , slice startOffset endOffset
  , slice endOffset endOfInput
  )
  where
    (startOffset, endOffset)
      | startLocation < endLocation = (startLocation, endLocation)
      | otherwise = (endLocation, startLocation)
      where
        startLocation :: Int
        startLocation = locationOffset start

        endLocation :: Int
        endLocation = locationOffset end

    locationOffset :: Location -> Int
    locationOffset location = advanceColumn (lineOffset location.line) location.column

    lineOffset :: Int -> Int
    lineOffset line = case drop (line - 1) lineOffsets of
      [] -> endOfInput
      offset : _ -> offset

    advanceColumn :: Int -> Int -> Int
    advanceColumn offset column
      | column < 1 = offset
      | columnOffset < 0 = endOfInput
      | otherwise = offset + columnOffset
      where
        columnOffset = T.measureOff (column - 1) (slice offset endOfInput)

    slice :: Int -> Int -> Text
    slice = sliceArray arr

    endOfInput :: Int
    endOfInput = startOfInput + len

    lineOffsets :: [Int]
    lineOffsets = startOfInput : map succ (indices "\n" input)

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst old@(Text _ _ needleLen) new input@(Text arr startOfInput len)
  | T.null old = new <> input
  | otherwise = case indices old input of
      [] -> input
      needle : _ -> mconcat [slice startOfInput needle, new, slice (needle + needleLen) endOfInput]
  where
    slice :: Int -> Int -> Text
    slice = sliceArray arr

    endOfInput :: Int
    endOfInput = startOfInput + len

sliceArray :: Array -> Int -> Int -> Text
sliceArray arr start end = Text arr start (end - start)

indices :: Text -> Text -> [Int]
indices needle haystack@(Text _ startOfInput _) = map (+ startOfInput) $ Search.indices needle haystack
