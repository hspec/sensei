{-# LANGUAGE CPP #-}
module GHC.Diagnostic (
  module Diagnostic
, Action(..)
, analyze
, apply
#ifdef TEST
, applyReplace
#endif
) where

import           Prelude hiding (span)

import           Imports

import           System.IO
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Builder (hPutBuilder)

import           GHC.Diagnostic.Type as Diagnostic

data Action = AddExtension FilePath Text | Replace Span Text
  deriving (Eq, Show)

analyze :: Diagnostic -> Maybe Action
analyze diagnostic = listToMaybe $ mapMaybe analyzeHint diagnostic.hints
  where
    analyzeHint :: String -> Maybe Action
    analyzeHint (T.pack -> hint) =
          perhapsYouIntendedToUse
      <|> enableAnyOfTheFollowingExtensions
      <|> perhapsUse
      <|> perhapsUseOneOfThese
      where
        perhapsYouIntendedToUse :: Maybe Action
        perhapsYouIntendedToUse = do
          AddExtension . (.file) <$> diagnostic.span <*> T.stripPrefix "Perhaps you intended to use " hint

        enableAnyOfTheFollowingExtensions :: Maybe Action
        enableAnyOfTheFollowingExtensions = do
          file <- (.file) <$> diagnostic.span
          T.stripPrefix "Enable any of the following extensions: " hint
            >>= listToMaybe . reverse . map (AddExtension file) . T.splitOn ", "

        perhapsUse :: Maybe Action
        perhapsUse = mkPerhapsUse "Perhaps use `"

        perhapsUseOneOfThese :: Maybe Action
        perhapsUseOneOfThese = mkPerhapsUse "Perhaps use one of these:\n  `"

        mkPerhapsUse :: Text -> Maybe Action
        mkPerhapsUse prefix = Replace <$> diagnostic.span <*> (takeIdentifier <$> T.stripPrefix prefix hint)
          where
            takeIdentifier :: Text -> Text
            takeIdentifier = T.takeWhile (/= '\'')

apply :: FilePath -> Action -> IO ()
apply dir = relativeTo dir >>> \ case
  AddExtension file name -> do
    old <- B.readFile file
    withFile file WriteMode $ \ h -> do
      hPutBuilder h $ "{-# LANGUAGE " <> T.encodeUtf8Builder name <> " #-}\n"
      B.hPutStr h old
  Replace span substitute -> do
    input <- B.readFile span.file <&> B.lines
    B.writeFile span.file . B.unlines $
      applyReplace span.start span.end substitute input

relativeTo :: FilePath -> Action -> Action
relativeTo dir = \ case
  AddExtension file name -> AddExtension (dir </> file) name
  Replace span substitute -> Replace span { file = dir </> span.file } substitute

applyReplace :: Location -> Location -> Text -> [ByteString] -> [ByteString]
applyReplace start end substitute input = case splitAt (start.line - 1) input of
  (xs, y : ys) | start.line == end.line -> xs ++ replaceInLine y : ys
  _ -> input
  where
    replaceInLine :: ByteString -> ByteString
    replaceInLine = T.decodeUtf8Lenient >>> T.splitAt (start.column - 1) >>> \ case
      (xs, ys) -> T.encodeUtf8 $ xs <> substitute <> T.drop (end.column - start.column) ys
