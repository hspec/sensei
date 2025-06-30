{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
module Builder (
  module Builder
, Color(..)
) where

import Imports hiding (join, unlines)

import Data.List qualified as List
import System.IO (Handle)
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Text.Encoding qualified as Text
import Data.Text.Internal.StrictBuilder qualified as StrictBuilder
import System.Console.ANSI

newtype Builder = Builder
#if MIN_VERSION_text(2,1,2)
  StrictBuilder.StrictTextBuilder
#else
  StrictBuilder.StrictBuilder
#endif
  deriving newtype (Semigroup, Monoid)

instance IsString Builder where
  fromString = Builder.fromString

fromString :: String -> Builder
fromString = fromText . Imports.fromString

fromText :: Text -> Builder
fromText = Builder . StrictBuilder.fromText

toText :: Builder -> Text
toText (Builder builder) = StrictBuilder.toText builder

readFile :: FilePath -> IO Builder
readFile name = fromText <$> Utf8.readFile name

hPutStr :: Handle -> Builder -> IO ()
hPutStr h = Utf8.hPutStr h . toText

show :: Show a => a -> Builder
show = Builder.fromString . Imports.show

join :: Builder -> [Builder] -> Builder
join sep = mconcat . List.intersperse sep

unlines :: [Builder] -> Builder
unlines = \ case
  [] -> mempty
  l : ls -> l <> "\n" <> unlines ls

withColor :: Color -> Builder -> Builder
withColor color = withSGR [SetColor Foreground Dull color]

withSGR :: [SGR] -> Builder -> Builder
withSGR sgr string = set <> string <> reset
  where
    set :: Builder
    set = Builder.fromString $ setSGRCode sgr

    reset :: Builder
    reset = Builder.fromString $ setSGRCode [Reset]

toByteString :: Builder -> ByteString
toByteString = Text.encodeUtf8 . Builder.toText
