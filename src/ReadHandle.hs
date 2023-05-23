{-# LANGUAGE CPP #-}
module ReadHandle (
  ReadHandle(..)
, toReadHandle
, marker
, getResult
, drain
#ifdef TEST
, newEmptyBuffer
#endif
) where

import           Imports

import qualified Data.ByteString.Char8 as B
import           Data.IORef
import           System.IO hiding (stdin, stdout, stderr, isEOF)

#if MIN_VERSION_bytestring(0,11,0)
import           Data.ByteString (dropEnd)
#else
import qualified Data.ByteString.Internal as B
dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps@(B.PS x offset len)
    | n <= 0    = ps
    | n >= len  = B.empty
    | otherwise = B.PS x offset (len - n)
#endif

-- | Truly random marker, used to separate expressions.
--
-- IMPORTANT: This module relies upon the fact that this marker is unique.  It
-- has been obtained from random.org.  Do not expect this module to work
-- properly, if you reuse it for any purpose!
marker :: ByteString
marker = pack (show @String "be77d2c8427d29cd1d62b7612d8e98cc") <> "\n"

partialMarkers :: [ByteString]
partialMarkers = reverse . drop 1 . init $ B.inits marker

data ReadHandle = ReadHandle {
  getChunk :: IO ByteString
, buffer :: IORef Buffer
}

drain :: ReadHandle -> (ByteString -> IO ()) -> IO ()
drain h echo = while (not <$> isEOF h) $ do
  _ <- getResult h echo
  pass

isEOF :: ReadHandle -> IO Bool
isEOF ReadHandle{..} = do
  readIORef buffer <&> \ case
    BufferEOF -> True
    BufferEmpty -> False
    BufferPartialMarker {}  -> False
    BufferChunk {} -> False

emptyBuffer :: Buffer -> Buffer
emptyBuffer old = case old of
  BufferEOF -> BufferEOF
  BufferEmpty -> BufferEmpty
  BufferPartialMarker {}  -> BufferEmpty
  BufferChunk {} -> BufferEmpty

mkBufferChunk :: ByteString -> Buffer
mkBufferChunk chunk
  | B.null chunk = BufferEmpty
  | otherwise = BufferChunk chunk

data Buffer =
    BufferEOF
  | BufferEmpty
  | BufferPartialMarker !ByteString
  | BufferChunk !ByteString

toReadHandle :: Handle -> Int -> IO ReadHandle
toReadHandle h n = do
  hSetBinaryMode h True
  ReadHandle (B.hGetSome h n) <$> newEmptyBuffer

newEmptyBuffer :: IO (IORef Buffer)
newEmptyBuffer = newIORef BufferEmpty

getResult :: ReadHandle -> (ByteString -> IO ()) -> IO ByteString
getResult h echo = mconcat <$> go
  where
    go :: IO [ByteString]
    go = nextChunk h >>= \ case
      Chunk chunk -> echo chunk >> (chunk :) <$> go
      Marker -> return []
      EOF -> return []

data Chunk = Chunk ByteString | Marker | EOF

nextChunk :: ReadHandle -> IO Chunk
nextChunk ReadHandle {..} = go
  where
    takeBuffer :: IO Buffer
    takeBuffer = atomicModifyIORef' buffer (emptyBuffer &&& id)

    putBuffer :: Buffer -> IO ()
    putBuffer = writeIORef buffer

    putBuffer_ :: ByteString -> IO ()
    putBuffer_ = putBuffer . mkBufferChunk

    getSome :: IO (Maybe ByteString)
    getSome = do
      chunk <- getChunk
      if B.null chunk then do
        putBuffer BufferEOF
        return Nothing
      else do
        return (Just chunk)

    go :: IO Chunk
    go = takeBuffer >>= \ case
      BufferEOF -> return EOF
      BufferEmpty -> getSome >>= \ case
        Nothing -> return EOF
        Just chunk -> processChunk chunk
      BufferPartialMarker partialMarker -> getSome >>= \ case
        Nothing -> return (Chunk partialMarker)
        Just chunk -> processChunk (partialMarker <> chunk)
      BufferChunk chunk -> processChunk chunk

    processChunk :: ByteString -> IO Chunk
    processChunk chunk = case stripMarker chunk of
      StrippedMarker rest -> do
        putBuffer_ rest
        return Marker
      PrefixBeforeMarker prefix rest -> do
        putBuffer_ rest
        return (Chunk prefix)
      NoMarker -> case splitPartialMarker chunk of
        Just (prefix, partialMarker) -> do
          putBuffer (BufferPartialMarker partialMarker)
          if B.null prefix then do
            go
          else do
            return (Chunk prefix)
        Nothing -> return (Chunk chunk)

splitPartialMarker :: ByteString -> Maybe (ByteString, ByteString)
splitPartialMarker chunk = split <$> findPartialMarker chunk
  where
    split partialMarker = (dropEnd (B.length partialMarker) chunk, partialMarker)

findPartialMarker :: ByteString -> Maybe ByteString
findPartialMarker chunk = find (`B.isSuffixOf` chunk) partialMarkers

data StripMarker =
    NoMarker
  | PrefixBeforeMarker !ByteString !ByteString
  | StrippedMarker !ByteString

stripMarker :: ByteString -> StripMarker
stripMarker input = case brakeAtMarker input of
  (_, "") -> NoMarker
  ("", dropMarker -> ys) -> StrippedMarker ys
  (xs, ys) -> PrefixBeforeMarker xs ys
  where
    brakeAtMarker = B.breakSubstring marker
    dropMarker = B.drop (B.length marker)
