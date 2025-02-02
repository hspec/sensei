module Config.DeepSeek where

import Imports

data DeepSeek = DeepSeek {
  auth :: BearerToken
} deriving (Eq, Show, Generic)

newtype BearerToken = BearerToken { bearer :: ByteString }
  deriving (Eq, Show)

instance FromJSON DeepSeek where
  parseJSON = genericKebabDecode

instance FromJSON BearerToken where
  parseJSON = fmap (BearerToken . encodeUtf8) . parseJSON
