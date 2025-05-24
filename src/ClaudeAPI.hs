{-# LANGUAGE DuplicateRecordFields #-}

module ClaudeAPI
( -- Configurations
  baseUrl
, defaultModel

  -- Send messages to Claude
, defaultChatRequest
, defaultCountTokenRequest
, defaultIOMediaChatRequest
, sendRequest
, chat
, addMessageToChatRequest
, addResponseToChatRequest
, chatBot
, countToken
, getMediaType
, encodeMediaToBase64
, addMediaToChatRequest

  -- Send message batches to Claude
, createMessageBatch
, retrieveMessageBatch
, listMessageBatch
, cancelMessageBatch
, retrieveMessageBatchResults

  -- Get model info
, listModels
, getModel
, defaultModelRequest

  -- Types
, MediaSource (..)
, RequestMessageContent (..)
, RequestMessage (..)
, ChatRequest (..)
, ResponseMessage (..)
, Usage (..)
, ChatResponse (..)
, ModelRequest (..)
, ModelData (..)
, ModelResponse (..)
, CountTokenRequest (..)
, CountTokenResponse (..)
, MessageBatchRequest (..)
, MessageBatchRequestCount (..)
, MessageBatchResponse (..)
, ListMessageBatchRequest (..)
, ListMessageBatchResponse (..)
, MessageBatchResult (..)
, RetrieveMessageBatchResult (..)

  -- Utility functions for building data types
, camelToUnderscore
, buildQueryString
)
where

import ClaudeAPI.Config (baseUrl, defaultModel)
import ClaudeAPI.Chat
    ( defaultChatRequest
    , defaultCountTokenRequest
    , defaultIOMediaChatRequest
    , sendRequest
    , chat
    , addMessageToChatRequest
    , addResponseToChatRequest
    , chatBot
    , countToken
    , getMediaType
    , encodeMediaToBase64
    , addMediaToChatRequest
    )
import ClaudeAPI.MessageBatches
    ( cancelMessageBatch
    , createMessageBatch
    , listMessageBatch
    , retrieveMessageBatch
    , retrieveMessageBatchResults
    )
import ClaudeAPI.Models (listModels, getModel, defaultModelRequest)
import ClaudeAPI.Types
    ( MediaSource (..)
    , RequestMessageContent (..)
    , RequestMessage (..)
    , ChatRequest (..)
    , ResponseMessage (..)
    , Usage (..)
    , ChatResponse (..)
    , ModelRequest (..)
    , ModelData (..)
    , ModelResponse (..)
    , CountTokenRequest (..)
    , CountTokenResponse (..)
    , MessageBatchRequest (..)
    , MessageBatchRequestCount (..)
    , MessageBatchResponse (..)
    , ListMessageBatchRequest (..)
    , ListMessageBatchResponse (..)
    , MessageBatchResult (..)
    , RetrieveMessageBatchResult (..)
    )
import ClaudeAPI.Utils
    ( camelToUnderscore
    , buildQueryString
    )
