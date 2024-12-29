{-# OPTIONS_GHC -Wno-orphans #-}

module ClaudeAPI.MessageBatches where

import ClaudeAPI.Chat (sendRequest)
import ClaudeAPI.Types 
    ( MessageBatchRequest (..)
    , MessageBatchRequests (..)
    , MessageBatchResponse (..)
    , ListMessageBatchRequest (..)
    , ListMessageBatchResponse (..)
    )
import ClaudeAPI.Utils (HasQueryParams (..), buildQueryString)


createMessageBatch :: MessageBatchRequests -> IO (Either String MessageBatchResponse)
createMessageBatch req = 
    sendRequest "POST" "/v1/messages/batches" (Just req)


retrieveMessageBatch :: String -> IO (Either String MessageBatchResponse)
retrieveMessageBatch messageBatchID = 
    sendRequest 
        "GET" 
        ("/v1/messages/batches/" ++ messageBatchID) 
        (Nothing :: Maybe MessageBatchRequests)


instance HasQueryParams ListMessageBatchRequest where 
    getBeforeID = beforeID
    getAfterID = afterID
    getLimit = limit

 
listMessageBatch 
    :: ListMessageBatchRequest 
    -> IO (Either String ListMessageBatchResponse)
listMessageBatch req =
    sendRequest 
        "GET" 
        ("/v1/messages/batches" ++ buildQueryString req)
        (Nothing :: Maybe ListMessageBatchRequest)


cancelMessageBatch :: String -> IO (Either String MessageBatchResponse)
cancelMessageBatch messageBatchID = 
    sendRequest
        "POST"
        ("/v1/messages/batches/" ++ messageBatchID ++ "/cancel") 
        (Nothing :: Maybe MessageBatchRequests)