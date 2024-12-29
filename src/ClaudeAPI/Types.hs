{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ClaudeAPI.Types where
import Data.Aeson 
import Data.Char (isUpper, toLower)
import Data.Text (Text)
import GHC.Generics (Generic)


camelToUnderscore :: String -> String
camelToUnderscore = concatMap toUnderscore
    where 
        toUnderscore c
            | isUpper c = ['_', toLower c]
            | otherwise = [c]


data ImageSource = ImageSource 
    { encodingType :: String
    , mediaType :: String
    , imageData :: Text
    } 
    deriving (Show, Generic)

instance ToJSON ImageSource where
    toJSON = genericToJSON defaultOptions 
        { omitNothingFields = True
        , fieldLabelModifier = 
            \case 
                "encodingType" -> "type"
                "mediaType" -> "media_type"
                "imageData" -> "data"
        }

data RequestMessageContent = 
    ImageContent { msgType :: String, source :: ImageSource } 
    | TextContent { msgType :: String, text :: String }
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

data RequestMessage = RequestMessage 
    { role :: String
    , content :: Either String [RequestMessageContent] 
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


data ChatRequest = ChatRequest 
    { model :: String
    , messages :: [RequestMessage]
    , max_tokens :: Int
    , stop_sequences :: Maybe String
    , stream :: Maybe Bool
    , system :: Maybe String
    , temperature :: Maybe Double
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
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }


data ResponseMessage = ResponseMessage { type_ :: String, responseText :: String }
    deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON = genericParseJSON defaultOptions 
        { fieldLabelModifier = 
            \case
                "type_" -> "type"
                "responseText" -> "text"
        }

data Usage = Usage 
    { input_tokens :: Int
    , cache_creation_input_tokens :: Int
    , cache_read_input_tokens :: Int
    , output_tokens :: Int
    }
    deriving (Show, Generic, FromJSON)

data ChatResponse = ChatResponse
    { id :: String
    , responseType :: String
    , responseRole :: String
    , responseContent :: [ResponseMessage]
    , responseModel :: String
    , stop_reason :: String
    , stop_sequence :: Maybe String
    , usage :: Usage
    }
    deriving (Generic)

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
            \x -> case x of
                "responseType" -> "type"
                "responseRole" -> "role"
                "responseContent" -> "content"
                "responseModel" -> "model"
                _ -> x
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

data ModelData = Model
    { modelType :: String
    , modelID :: String
    , displayName :: String
    , createdAt :: String
    } 
    deriving (Show, Generic)

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
    deriving (Show, Generic)

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
    deriving (Generic, Show)

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
    deriving (Show, Generic)

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
    deriving (Show, Generic)

instance FromJSON ListMessageBatchResponse where
    parseJSON = genericParseJSON defaultOptions 
        { fieldLabelModifier = 
            \case 
                "responseData" -> "data"
        }