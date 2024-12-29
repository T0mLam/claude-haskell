module ClaudeAPI.Utils where 

import Data.Char (isUpper, toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes)


class HasQueryParams a where 
    getBeforeID :: a -> Maybe String
    getAfterID  :: a -> Maybe String
    getLimit    :: a -> Maybe Int


buildQueryString :: HasQueryParams a => a -> String
buildQueryString req =
    case params of
        [] -> ""
        _ -> "?" ++ intercalate "&" params
        where
            params = catMaybes
                [ fmap ("before_id=" ++) (getBeforeID req)
                , fmap ("after_id=" ++) (getAfterID req)
                , fmap (\v -> "limit=" ++ show v) (getLimit req)
                ]

    
camelToUnderscore :: String -> String
camelToUnderscore = concatMap toUnderscore
    where 
        toUnderscore c
            | isUpper c = ['_', toLower c]
            | otherwise = [c]