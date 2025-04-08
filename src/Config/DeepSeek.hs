module Config.DeepSeek where

import Imports

data DeepSeek = DeepSeek {
  auth :: BearerToken
} deriving (Eq, Show, Generic)
  deriving FromJSON via (KebabOptions DeepSeek)

newtype BearerToken = BearerToken { bearer :: ByteString }
  deriving (Eq, Show)

instance FromJSON BearerToken where
  parseJSON = fmap (BearerToken . encodeUtf8) . parseJSON
