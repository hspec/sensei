{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LimitErrorOutput (
  Lines(..)
, Columns(..)
, limitToTerminalSize
#ifdef TEST
, requiredScreenEstate
, wcwidth
, parseLocation
#endif
) where

import           Prelude hiding (read)
import           Imports
import           Data.IORef
import           Foreign.C
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Test.Hspec.Core.Util (stripAnsi)

newtype Lines = Lines { unLines :: Int }
  deriving newtype (Eq, Show, Ord, Bounded, Enum, Num, Real, Integral)

newtype Columns = Columns { unColumns :: Int }
  deriving newtype (Eq, Show, Ord, Bounded, Enum, Num, Real, Integral)

data OutputMode = InformationalOutput | ErrorOutput

limitToTerminalSize :: Lines -> Columns -> (ByteString -> IO ()) -> IO (ByteString -> IO ())
-- NOTES:
--
-- 1. Once we see the first error, we switch to line buffering.  This means
--    that progress reporting will not properly work after the first error. In
--    practice this is not an issue as Hspec does not use progress reporting
--    during error output, only during informational output.  However, it is
--    important to not use line buffering before the first error as to not
--    impact progress reporting during informational output.
--
-- 2. We never output partial error lines. We either output a given line as a
--    whole, or if there is not enough screen estate left, not at all.
--
-- 3. We always output at least one error line, regardless of how much screen
--    estate is left.
--
limitToTerminalSize terminalHeight terminalWidth echo = do
  outputMode <- newIORef InformationalOutput
  remainingBudget <- newIORef terminalHeight
  currentPartialLine <- newIORef ""

  let
    unbufferedEcho :: ByteString -> IO ()
    unbufferedEcho chunk = readIORef outputMode >>= \ case
      InformationalOutput -> echo chunk
      ErrorOutput -> pass

    onOutputLine :: (ByteString -> IO ()) -> ByteString -> IO ()
    onOutputLine action = fix $ \ rec chunk -> case breakAtNewline chunk of
      Nothing -> do
        unbufferedEcho chunk
        modifyIORef currentPartialLine (<> chunk)
      Just (newlineTerminatedChunk, rest) -> do
        unbufferedEcho newlineTerminatedChunk
        currentLine <- readIORef currentPartialLine <&> (<> newlineTerminatedChunk)
        action currentLine
        writeIORef currentPartialLine ""
        rec rest

    limitedEcho :: ByteString -> IO ()
    limitedEcho = onOutputLine $ \ currentLine -> do
      readIORef outputMode >>= \ case
        InformationalOutput -> do
          when (isErrorMessage currentLine) $ do
            writeIORef outputMode ErrorOutput
            required <- requiredScreenEstate terminalWidth currentLine
            modifyIORef remainingBudget (subtract required)
        ErrorOutput -> do
          budget <- readIORef remainingBudget
          required <- requiredScreenEstate terminalWidth currentLine
          when (budget >= required) $ do
            echo currentLine
          modifyIORef remainingBudget (subtract required)

  return limitedEcho

requiredScreenEstate :: Columns -> ByteString -> IO Lines
requiredScreenEstate (Columns terminalWidth) = fmap requiredLines . mapM wcwidth . stripAnsi . T.unpack . T.decodeUtf8Lenient . discardTrailingNewline
  where
    requiredLines :: [Int] -> Lines
    requiredLines xs
      | any (< 0) xs = 1
      | otherwise = go 0 xs

    go :: Int -> [Int] -> Lines
    go acc = \ case
      [] -> 1
      x : xs | currentLine <- acc + x, currentLine <= terminalWidth -> go currentLine xs
      x : xs -> succ $ go x xs

discardTrailingNewline :: ByteString -> ByteString
discardTrailingNewline line = fromMaybe line $ B.stripSuffix "\n" line

wcwidth :: Char -> IO Int
wcwidth = fmap fromEnum . c_wcwidth . toEnum . fromEnum

foreign import ccall unsafe "wchar.h wcwidth" c_wcwidth :: CWchar -> IO CInt

breakAtNewline :: ByteString -> Maybe (ByteString, ByteString)
breakAtNewline chunk = case B.break (== '\n') chunk of
  (_, "") -> Nothing
  (xs, ys) -> Just (xs <> "\n", B.drop 1 ys)

isErrorMessage :: ByteString -> Bool
isErrorMessage = isJust . parseLocation

parseLocation :: ByteString -> Maybe (ByteString, Int, Int)
parseLocation input = case fmap (fmap breakColon . breakColon) (breakColon input) of
  (file, (line, (column, _))) -> (,,) file <$> read line <*> read column
  where
    read :: Read a => ByteString -> Maybe a
    read = either (const Nothing) Just . T.decodeUtf8' >=> readMaybe . T.unpack

    breakColon :: ByteString -> (ByteString, ByteString)
    breakColon = fmap (B.drop 1) . B.break (== ':')
