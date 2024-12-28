module ClaudeAPI.Models where

import ClaudeAPI.Chat (sendRequest)
import ClaudeAPI.Types (ModelRequest(..), ModelResponse(..), ModelData(..))

import Data.List (intercalate)
import Data.Maybe (catMaybes)


defaultModelRequest :: ModelRequest
defaultModelRequest =
    ModelRequest
        { beforeID = Nothing
        , afterID = Nothing
        , limit = Nothing
        }


buildQueryString :: ModelRequest -> String
buildQueryString req =
    case params of
        [] -> ""
        _ -> "?" ++ intercalate "&" params
        where
            params = catMaybes
                [ fmap ("before_id=" ++) (beforeID req)
                , fmap ("after_id=" ++) (afterID req)
                , fmap (\v -> "limit=" ++ show v) (limit req)
                ]


listModels :: ModelRequest -> IO (Either String ModelResponse)
listModels req = 
    sendRequest "GET" ("/v1/models" ++ buildQueryString req) (Nothing :: Maybe ModelRequest)


getModel :: String -> IO (Either String ModelData)
getModel modelId = sendRequest "GET" ("/v1/models/" ++ modelId) (Nothing :: Maybe ModelRequest)