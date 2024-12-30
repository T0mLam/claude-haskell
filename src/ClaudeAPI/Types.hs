{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ClaudeAPI.Types where

import ClaudeAPI.Utils (camelToUnderscore)

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL


-- | A class representing types that can be decoded from JSON responses.
class FromJSON a => JSONResponse a where
    parseResponse :: BL.ByteString -> Either String a
    parseResponse = eitherDecode

-- | Represents an image source with metadata.
data MediaSource = MediaSource
    { encodingType :: String
    -- ^ The encoding type of the image (e.g., "base64").
    , mediaType :: String
    -- ^ The MIME type of the image (e.g., "image/png").
    , imageData :: Text
    -- ^ The actual image data
    }
    deriving (Show, Generic)

instance ToJSON MediaSource where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True
        , fieldLabelModifier =
            \case
                "encodingType" -> "type"
                "mediaType" -> "media_type"
                "imageData" -> "data"
        }

-- | Represents the content of a request message, which can be text or an image.
data RequestMessageContent =
    MediaContent { msgType :: String, source :: MediaSource }
    -- ^ Content for an image message.
    | TextContent { msgType :: String, text :: String }
    -- ^ Content for a text message.
    deriving (Show, Generic)

instance ToJSON RequestMessageContent where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True
        , sumEncoding = UntaggedValue
        , fieldLabelModifier =
            \case
                "msgType" -> "type"
                other -> other
        }

-- | Represents a request message sent to the API.
data RequestMessage = RequestMessage
    { role :: String
    -- ^ The role of the message sender (e.g., "user").
    , content :: Either String [RequestMessageContent]
    -- ^ The message content (string or list of structured content).
    }
    deriving (Show, Generic)

instance ToJSON RequestMessage where
    toJSON (RequestMessage role_ content_) =
        object
            [ "role" .= role_
            , "content" .=
                case content_ of
                    Left str -> toJSON str
                    Right msgs -> toJSON msgs
            ]

-- | Represents a chat request containing multiple messages and metadata.
data ChatRequest = ChatRequest
    { model :: String
    -- ^ The name of the model to use.
    , messages :: [RequestMessage]
    -- ^ The list of messages in the chat.
    , maxTokens :: Int
    -- ^ The maximum number of tokens to generate.
    , stopSequences :: Maybe String
    -- ^ Optional stop sequences for the response.
    , stream :: Maybe Bool
    -- ^ Whether to stream the response.
    , system :: Maybe String
    -- ^ Optional system configuration.
    , temperature :: Maybe Double
    -- ^ Temperature for the response.
    }
    deriving (Generic)

instance Show ChatRequest where
    show (ChatRequest model_ messages_ max_tokens_ stop_sequences_ stream_ system_ temperature_) =
        unlines
            [ "\n---ChatRequest---"
            , "Model:", show model_
            , "\nMessages:", show messages_
            , "\nMax tokens:", show max_tokens_
            , "\nStop sequences:", show stop_sequences_
            , "\nStream:", show stream_
            , "\nSystem:", show system_
            , "\nTemperature:", show temperature_
            ]

instance ToJSON ChatRequest where
    toJSON = genericToJSON defaultOptions 
        { omitNothingFields = True 
        , fieldLabelModifier = camelToUnderscore 
        }

data ResponseMessage = ResponseMessage { responseType :: String, responseText :: String }
    deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "responseType" -> "type"
                "responseText" -> "text"
        }

data Usage = Usage
    { inputTokens :: Int
    , cacheCreationInputTokens :: Int
    , cacheReadInputTokens :: Int
    , outputTokens :: Int
    }
    deriving (Show, Generic)

instance FromJSON Usage where 
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelToUnderscore }

-- | Represents the full chat response from the API.
data ChatResponse = ChatResponse
    { id :: String
    -- ^ The unique identifier for the response.
    , responseType :: String
    -- ^ The type of the response.
    , responseRole :: String
    -- ^ The role of the responder (e.g., "assistant").
    , responseContent :: [ResponseMessage]
    -- ^ The content of the response.
    , responseModel :: String
    -- ^ The model used to generate the response.
    , stopReason :: String
    -- ^ The reason for stopping the response generation.
    , stopSequence :: Maybe String
    -- ^ The stop sequence that triggered the stop, if any.
    , usage :: Usage
    -- ^ Token usage details.
    }
    deriving (Generic, JSONResponse)

instance Show ChatResponse where
    show (ChatResponse id_ type__ role_ content_ model_ stop_reason_ stop_sequence_ usage_) =
        unlines
            [ "\n---ChatResponse---"
            , "ID:", show id_
            , "\nType:", show type__
            , "\nRole:", show role_
            , "\nContent:", show content_
            , "\nModel:", show model_
            , "\nStop reason:", show stop_reason_
            , "\nStop sequence:", show stop_sequence_
            , "\nUsage:", show usage_
            ]

instance FromJSON ChatResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "responseType" -> "type"
                "responseRole" -> "role"
                "responseContent" -> "content"
                "responseModel" -> "model"
                other -> camelToUnderscore other
        }


data ModelRequest = ModelRequest
    { beforeID :: Maybe String
    , afterID :: Maybe String
    , limit :: Maybe Int
    }
    deriving (Show, Generic)

instance ToJSON ModelRequest where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True
        , fieldLabelModifier =
            \case
                "beforeID" -> "before_id"
                "afterID" -> "after_id"
        }

data ModelData = ModelData
    { modelType :: String
    , modelID :: String
    , displayName :: String
    , createdAt :: String
    }
    deriving (Show, Generic, JSONResponse)

instance FromJSON ModelData where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "modelType" -> "type"
                "modelID" -> "id"
                "displayName" -> "display_name"
                "createdAt" -> "created_at"
        }

data ModelResponse = ModelResponse
    { modelData :: [ModelData]
    , hasMore :: Bool
    , firstID :: Maybe String
    , lastID :: Maybe String
    }
    deriving (Show, Generic, JSONResponse)

instance FromJSON ModelResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "modelData" -> "data"
                "hasMore" -> "has_more"
                "firstID" -> "first_id"
                "lastID" -> "last_id"
        }


data CountTokenRequest = CountTokenRequest
    { requestMessages :: [RequestMessage]
    , model :: String
    , system :: Maybe String
    }
    deriving (Generic, Show)

instance ToJSON CountTokenRequest where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True
        , fieldLabelModifier =
            \case
                "requestMessages" -> "messages"
                other -> other
        }


newtype CountTokenResponse =
    CountTokenResponse { inputTokens :: Int }
    deriving (Generic, Show, JSONResponse)

instance FromJSON CountTokenResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "inputTokens" -> "input_tokens"
        }


-- Message Batch Types
data MessageBatchRequest = MessageBatchRequest
    { customID :: String
    , params :: ChatRequest
    }
    deriving (Show, Generic)

instance ToJSON MessageBatchRequest where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier =
            \case
                "customID" -> "custom_id"
                other -> other
        }

newtype MessageBatchRequests =
    MessageBatchRequests { requests :: [MessageBatchRequest]}
    deriving (Show, Generic, ToJSON)

data MessageBatchRequestCount = MessageBatchRequestCount
    { processing :: Int
    , succeeded :: Int
    , errored :: Int
    , canceled :: Int
    , expired :: Int
    } deriving (Show, Generic, FromJSON)

data MessageBatchResponse = MessageBatchResponse
    { id :: String
    , responseType :: String
    , processingStatus :: String
    , requestCounts :: MessageBatchRequestCount
    , endedAt :: Maybe String
    , createdAt :: String
    , expiresAt :: String
    , archivedAt :: Maybe String
    , cancelInitiatedAt :: Maybe String
    , resultsUrl :: Maybe String
    }
    deriving (Show, Generic, JSONResponse)

instance FromJSON MessageBatchResponse where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True
        , fieldLabelModifier =
            \case
                "responseType" -> "type"
                other -> camelToUnderscore other
        }

data ListMessageBatchRequest = ListMessageBatchRequest
    { beforeID :: Maybe String
    , afterID :: Maybe String
    , limit :: Maybe Int
    }
    deriving (Show, Generic)

instance ToJSON ListMessageBatchRequest where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True
        , fieldLabelModifier =
            \case
                "beforeID" -> "before_id"
                "afterID" -> "after_id"
        }

newtype ListMessageBatchResponse =
    ListMessageBatchResponse { responseData :: [MessageBatchResponse] }
    deriving (Show, Generic, JSONResponse)

instance FromJSON ListMessageBatchResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "responseData" -> "data"
        }

data MessageBatchResult = MessageBatchResult
    { resultType :: String
    , message :: ChatResponse
    }
    deriving (Show, Generic)

instance FromJSON MessageBatchResult where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "resultType" -> "type"
                other -> other
        }

data RetrieveMessageBatchResult = RetrieveMessageBatchResult
    { customID :: String
    , result :: MessageBatchResult
    }
    deriving (Show, Generic)

instance FromJSON RetrieveMessageBatchResult where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier =
            \case
                "customID" -> "custom_id"
                other -> other
        }

newtype RetrieveMessageBatchResults =
    RetrieveMessageBatchResults [RetrieveMessageBatchResult]
    deriving (Show, Generic, FromJSON)

instance JSONResponse RetrieveMessageBatchResults where
    parseResponse input =
        let jsonLines = BL.lines input
        in RetrieveMessageBatchResults <$> mapM eitherDecode jsonLines