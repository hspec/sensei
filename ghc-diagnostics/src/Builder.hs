{-# LANGUAGE CPP #-}
module Builder where

import Imports hiding (join, unlines)

import Data.List qualified as List
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

show :: Show a => a -> Builder
show = Builder.fromString . Imports.show

join :: Builder -> [Builder] -> Builder
join sep = mconcat . List.intersperse sep

unlines :: [Builder] -> Builder
unlines = \ case
  [] -> mempty
  l : ls -> l <> "\n" <> unlines ls

withSGR :: [SGR] -> Builder -> Builder
withSGR sgr string = set <> string <> reset
  where
    set :: Builder
    set = Builder.fromString $ setSGRCode sgr

    reset :: Builder
    reset = Builder.fromString $ setSGRCode [Reset]
