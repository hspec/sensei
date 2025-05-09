{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
module Builder where

import Imports

import Data.List qualified as List
import Data.ByteString qualified as ByteString
import Data.Text.Encoding qualified as Text
import Data.Text.Internal.StrictBuilder qualified as StrictBuilder

#if MIN_VERSION_text(2,1,2)
newtype Builder = Builder StrictBuilder.StrictTextBuilder
#else
newtype Builder = Builder StrictBuilder.StrictBuilder
#endif
  deriving newtype (Semigroup, Monoid)

instance IsString Builder where
  fromString = fromText . fromString

fromText :: Text -> Builder
fromText = Builder . StrictBuilder.fromText

toText :: Builder -> Text
toText (Builder builder) = StrictBuilder.toText builder

readFile :: FilePath -> IO Builder
readFile name = do
  c <- ByteString.readFile name
  case ByteString.isValidUtf8 c of
    True -> return . Builder $ StrictBuilder.unsafeFromByteString c
    False -> either throwIO (return . fromText) $ Text.decodeUtf8' c

show :: Show a => a -> Builder
show = fromString . Imports.show

join :: Builder -> [Builder] -> Builder
join sep = mconcat . List.intersperse sep
