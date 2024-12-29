{-# OPTIONS_GHC -Wno-orphans #-}
module ClaudeAPI.Models where

import ClaudeAPI.Chat (sendRequest)
import ClaudeAPI.Types 
    ( ModelRequest(..)
    , ModelResponse(..)
    , ModelData(..)
    )
import ClaudeAPI.Utils 
    ( buildQueryString
    , HasQueryParams (..)
    )


defaultModelRequest :: ModelRequest
defaultModelRequest =
    ModelRequest
        { beforeID = Nothing
        , afterID = Nothing
        , limit = Nothing
        }


instance HasQueryParams ModelRequest where 
    getBeforeID = beforeID
    getAfterID = afterID
    getLimit = limit


listModels :: ModelRequest -> IO (Either String ModelResponse)
listModels req = 
    sendRequest "GET" ("/v1/models" ++ buildQueryString req) (Nothing :: Maybe ModelRequest)


getModel :: String -> IO (Either String ModelData)
getModel modelId = sendRequest "GET" ("/v1/models/" ++ modelId) (Nothing :: Maybe ModelRequest)