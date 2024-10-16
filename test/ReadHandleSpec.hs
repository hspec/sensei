module ReadHandleSpec (spec) where

import           Helper

import           Test.QuickCheck
import qualified Data.ByteString as ByteString

import           Session (Summary(..), extractSummary)
import           Language.Haskell.GhciWrapper (extractReloadStatus)

import           ReadHandle

chunkByteString :: (Int, Int) -> ByteString -> Gen [ByteString]
chunkByteString size = go
  where
    go "" = return []
    go xs = do
      n <- chooseInt size
      let (chunk, rest) = ByteString.splitAt n xs
      (chunk :) <$> go rest

fakeHandle :: [ByteString] -> IO ReadHandle
fakeHandle chunks = ReadHandle <$> stubAction chunks <*> newEmptyBuffer

data ChunkSizes = SmallChunks | BigChunks

withRandomChunkSizes :: [ByteString] -> (ReadHandle -> Expectation) -> Property
withRandomChunkSizes (mconcat -> input) action = property $ do
  chunkSizes <- elements [SmallChunks, BigChunks]
  let
    maxChunkSize :: Int
    maxChunkSize = case chunkSizes of
      SmallChunks -> 4
      BigChunks -> ByteString.length input

  chunks <- chunkByteString (1, maxChunkSize) input
  return $ do
    counterexample (unlines $ map show chunks) $ do
      fakeHandle chunks >>= action

partialMarker :: ByteString
partialMarker = ByteString.take 5 marker

spec :: Spec
spec = do
  describe "breakAfterNewLine" $ do
    it "brakes after newline" $ do
      breakAfterNewLine "foo\nbar\nbaz" `shouldBe` Just ("foo\n", "bar\nbaz")

  describe "drain" $ do
    it "drains all remaining input" $ do
      h <- fakeHandle ["foo", marker, "bar", marker, "baz", marker, ""]
      withSpy (drain extractReloadStatus h) `shouldReturn` ["foo", "bar", "baz"]

  describe "getResult" $ do
    context "with a single result" $ do
      let input = ["foo", "bar", "baz", marker]

      it "returns result" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle input
          getResult extractReloadStatus h echo `shouldReturn` ("foobarbaz", [])
        `shouldReturn` ["foo", "bar", "baz"]

      context "with chunks of arbitrary size" $ do
        it "returns result" $ do
          withRandomChunkSizes input $ \ h -> do
            fmap mconcat . withSpy $ \ echo -> do
              getResult extractReloadStatus h echo `shouldReturn` ("foobarbaz", [])
            `shouldReturn` "foobarbaz"

      context "with extractSummary" $ do
        let
          summary :: Summary
          summary = Summary 5 3

        it "extracts Summary" $ do
          let
            chunks :: [ByteString]
            chunks = [
                "foo\n"
              , "bar\n"
              , encodeUtf8 $ show summary <> "\n"
              , "baz\n"
              , marker
              ]

          withRandomChunkSizes chunks $ \ h -> do
            fmap mconcat . withSpy $ \ echo -> do
              getResult extractSummary h echo `shouldReturn` ("foo\nbar\nbaz\n", [summary])
            `shouldReturn` "foo\nbar\nbaz\n"

        context "when predicate does not match" $ do
          it "does not attempt parsing" $ do
            let
              extract :: Extract Summary
              extract = extractSummary {parseMessage = undefined}

              chunks :: [ByteString]
              chunks = [
                  "foo\n"
                , "bar\n"
                , encodeUtf8 . take 8 $ show summary <> "\n"
                , "baz\n"
                , marker
                ]

            withRandomChunkSizes chunks $ \ h -> do
              fmap mconcat . withSpy $ \ echo -> do
                getResult extract h echo `shouldReturn` ("foo\nbar\nSummary baz\n", [])
              `shouldReturn` "foo\nbar\nSummary baz\n"

    context "with multiple results" $ do
      let input = ["foo", marker, "bar", marker, "baz", marker]

      it "returns one result at a time" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle input
          getResult extractReloadStatus h echo `shouldReturn` ("foo", [])
          getResult extractReloadStatus h echo `shouldReturn` ("bar", [])
          getResult extractReloadStatus h echo `shouldReturn` ("baz", [])
        `shouldReturn` ["foo", "bar", "baz"]

      context "with chunks of arbitrary size" $ do
        it "returns one result at a time" $ do
          withRandomChunkSizes input $ \ h -> do
            fmap mconcat . withSpy $ \ echo -> do
              getResult extractReloadStatus h echo `shouldReturn` ("foo", [])
              getResult extractReloadStatus h echo `shouldReturn` ("bar", [])
              getResult extractReloadStatus h echo `shouldReturn` ("baz", [])
            `shouldReturn` "foobarbaz"

    context "when a chunk that contains a marker ends with a partial marker" $ do
      it "correctly gives the marker precedence over the partial marker" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle ["foo" <> marker <> "bar" <> partialMarker, ""]
          getResult extractReloadStatus h echo `shouldReturn` ("foo", [])
          getResult extractReloadStatus h echo `shouldReturn` ("bar" <> partialMarker, [])
        `shouldReturn` ["foo", "bar", partialMarker]

    context "on EOF" $ do
      it "returns all remaining input" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle ["foo", "bar", "baz", ""]
          getResult extractReloadStatus h echo `shouldReturn` ("foobarbaz", [])
        `shouldReturn` ["foo", "bar", "baz"]

      context "with a partialMarker at the end" $ do
        it "includes the partial marker in the output" $ do
          withSpy $ \ echo -> do
            h <- fakeHandle ["foo", "bar", "baz", partialMarker, ""]
            getResult extractReloadStatus h echo `shouldReturn` ("foobarbaz" <> partialMarker, [])
          `shouldReturn` ["foo", "bar", "baz", partialMarker]

      context "after a marker" $ do
        it "returns all remaining input" $ do
          withSpy $ \ echo -> do
            h <- fakeHandle ["foo", "bar", "baz", marker, "qux", ""]
            getResult extractReloadStatus h echo `shouldReturn` ("foobarbaz", [])
            getResult extractReloadStatus h echo `shouldReturn` ("qux", [])
          `shouldReturn` ["foo", "bar", "baz", "qux"]
