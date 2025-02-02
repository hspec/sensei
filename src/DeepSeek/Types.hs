{-# LANGUAGE DeriveAnyClass #-}
module DeepSeek.Types (
-- * Request
  CreateChatCompletion(..)
, Message(..)
, Role(..)

-- * Response
, ChatCompletion(..)
, Choice(..)
, FinishReason(..)
) where

import Imports

import Data.Aeson
import Data.Time.Clock.POSIX

data CreateChatCompletion = CreateChatCompletion {
  messages :: [Message]
, model :: String
, temperature :: Maybe Double
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Message = Message {
  content :: String
, role :: Role
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Role =
    System
  | User
  | Assistant
  deriving (Eq, Show)

instance ToJSON Role where
  toJSON = \ case
    System -> "system"
    User -> "user"
    Assistant -> "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" \ case
    "system" -> return System
    "user" -> return User
    "assistant" -> return Assistant
    value -> fail ("invalid value " <> show value)

data ChatCompletion = ChatCompletion {
  id :: String
, choices :: [Choice]
, created :: POSIXTime
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Choice = Choice {
  finish_reason :: FinishReason
, index :: Int
, message :: Message
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data FinishReason =
    Stop
  | Length
  | ContentFilter
  | ToolCalls
  | InsufficientSystemResource
  deriving (Eq, Show)

instance ToJSON FinishReason where
  toJSON = \ case
    Stop -> "stop"
    Length -> "length"
    ContentFilter -> "content_filter"
    ToolCalls -> "tool_calls"
    InsufficientSystemResource -> "insufficient_system_resource"

instance FromJSON FinishReason where
  parseJSON = withText "FinishReason" \ case
    "stop" -> return Stop
    "length" -> return Length
    "content_filter" -> return ContentFilter
    "tool_calls" -> return ToolCalls
    "insufficient_system_resource" -> return InsufficientSystemResource
    value -> fail ("invalid value " <> show value)
