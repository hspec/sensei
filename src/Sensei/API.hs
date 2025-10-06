module Sensei.API (
  get
, post

, modules
, config
, trigger
, quickFix
, quickFixAll
, deepFix

, Modules(..)

, Config(..)

, QuickFixRequest(..)

, DeepFixRequest(..)
, Instructions(..)
, Span(..)
, Location(..)
) where

import Imports

import Network.HTTP.Client
import Data.ByteString.Builder qualified as Builder
import Data.Aeson qualified as Aeson

import GHC.Diagnostic.Type (Span(..), Location(..))
import HTTP.Util (makeRequest)

modules :: FilePath -> IO (Either LazyByteString Modules)
modules = get "/modules"

data Modules = Modules {
  modules :: [String]
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions Modules)

config :: FilePath -> IO (Either LazyByteString Config)
config = get "/config"

data Config = Config {
  hieDir :: FilePath
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions Config)

trigger :: FilePath -> IO (Bool, LazyByteString)
trigger dir = post "/trigger" dir $ Aeson.object []

data QuickFixRequest = QuickFixRequest {
  choice :: Maybe Int
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions QuickFixRequest)

quickFix :: FilePath -> QuickFixRequest -> IO (Bool, LazyByteString)
quickFix = post "/quick-fix"

quickFixAll :: FilePath -> IO (Bool, LazyByteString)
quickFixAll dir = post "/quick-fix-all" dir Aeson.Null

data DeepFixRequest = DeepFixRequest {
  instructions :: Maybe Instructions
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions DeepFixRequest)

data Instructions = Instructions {
  span :: Span
, instructions :: Text
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions Instructions)

deepFix :: FilePath -> DeepFixRequest -> IO (Bool, LazyByteString)
deepFix = post "/deep-fix"

post :: ToJSON a => String -> FilePath -> a -> IO (Bool, LazyByteString)
post endpoint dir (RequestBodyLBS . Aeson.encode -> requestBody) = makeRequest dir request
  where
    request :: Request
    request = (mkRequest endpoint) { method = "POST", requestBody }

get :: FromJSON a => String -> FilePath -> IO (Either LazyByteString a)
get endpoint dir = makeRequest dir request <&> \ case
  (False, body) -> Left body
  (True, body) -> first (Builder.toLazyByteString . Builder.stringUtf8) $ Aeson.eitherDecode body
  where
    request :: Request
    request = mkRequest endpoint

mkRequest :: FilePath -> Request
mkRequest endpoint = (fromString $ "http://localhost" <> endpoint)
