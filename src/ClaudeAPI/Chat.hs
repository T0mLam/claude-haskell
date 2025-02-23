{-# LANGUAGE DuplicateRecordFields #-}

module ClaudeAPI.Chat where

import ClaudeAPI.Types
    ( ChatResponse (..)
    , ChatRequest (..)
    , RequestMessage (..)
    , RequestMessageContent (..)
    , ResponseMessage (..)
    , CountTokenRequest (..)
    , CountTokenResponse (..)
    , MediaSource (..)
    , JSONResponse (..)
    )
import ClaudeAPI.Config (baseUrl, defaultModel)

import Configuration.Dotenv (loadFile, defaultConfig)  
import Control.Exception (SomeException, try)
import Data.Aeson (encode, ToJSON)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client 
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, Status (statusMessage))
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.CaseInsensitive as CI


defaultChatRequest :: String -> ChatRequest
defaultChatRequest reqContent = ChatRequest 
    { model = defaultModel
    , messages = [RequestMessage { role = "user", content = Left reqContent }]
    , maxTokens = 1024
    , stopSequences = Nothing
    , stream = Nothing
    , system = Nothing
    , temperature = Nothing
    }


-- Send any request to the Claude API
sendRequest
    :: (ToJSON req, JSONResponse resp) 
    => String -- request method
    -> String -- endpoint
    -> Maybe req 
    -> IO (Either String resp)
sendRequest requestMethod endpoint chatReq = do
    -- Try to load the API key from the .env file
    envFileExists <- doesFileExist ".env"
    when envFileExists $ loadFile defaultConfig

    getApiKey <- try (getEnv "API_KEY") :: IO (Either SomeException String)
    getAnthropicVersion <- try (getEnv "ANTHROPIC_VERSION") :: IO (Either SomeException String)

    case (getApiKey, getAnthropicVersion) of 
        (Left _, _) -> return $ Left "error: API key not found"
        (_, Left _) -> return $ Left "error: Anthropic version not found"
        (Right apiKey, Right anthropicVersion) -> do
            -- Create a new HTTP manager
            manager <- newManager tlsManagerSettings

            -- Parse the base URL
            initialRequest <- parseRequest $ baseUrl ++ endpoint

            -- Encode the request as JSON
            let claudeRequestBody = maybe mempty (RequestBodyLBS . encode) chatReq

            -- Create the request object 
            let request = initialRequest
                    { requestHeaders = fmap (\(k, v) -> (CI.mk $ BS.pack k, BS.pack v))
                        [ ("Content-Type", "application/json")
                        , ("x-api-key", apiKey)
                        , ("anthropic-version", anthropicVersion)
                        ]
                    , method = BS.pack requestMethod
                    , requestBody = claudeRequestBody
                    }

            -- Send the request and extract the response status and body
            response <- httpLbs request manager

            let responseStatus' = responseStatus response
            let responseBody' = responseBody response

            case statusCode responseStatus' of
                200 -> case parseResponse responseBody' of
                    -- Decode the json or jsonl response into data object
                    Right responseBodyObject -> return $ Right responseBodyObject
                    Left err -> return $ Left $ "error: failed to decode response body" ++ err
                    
                code -> do 
                    -- Decode the error response body into a string
                    let errorDetails = BL.unpack responseBody'
                    let responseStatusMessage' = statusMessage responseStatus'
                    return $ 
                        Left $ 
                            "error " ++ show code ++ ": " 
                            ++ BS.unpack responseStatusMessage' 
                            ++ ". " ++ errorDetails


chat :: ChatRequest -> IO (Either String ChatResponse)
chat req = sendRequest "POST" "/v1/messages" (Just req)
 

addMessageToChatRequest :: String -> String -> ChatRequest -> ChatRequest
addMessageToChatRequest r m req = 
    req { messages = messages req ++ [RequestMessage { role = r, content = Left m }] }


addResponseToChatRequest :: ChatRequest -> ChatResponse -> ChatRequest
addResponseToChatRequest req resp = 
    let 
        respRole = responseRole resp
        respContent = responseText $ head $ responseContent resp
    in 
        addMessageToChatRequest respRole respContent req

    
defaultCountTokenRequest :: String -> CountTokenRequest
defaultCountTokenRequest reqContent = CountTokenRequest 
    { model = defaultModel
    , requestMessages = [RequestMessage { role = "user", content = Left reqContent }]
    , system = Nothing
    }

    
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)
countToken req = sendRequest "POST" "/v1/messages/count_tokens" (Just req)


getMediaType :: FilePath -> String
getMediaType mediaPath =
    case map toLower (takeExtension mediaPath) of 
        ".pdf" ->  "application/pdf"
        ".jpg" -> "image/jpeg"
        other -> "image/" ++ drop 1 other


encodeMediaToBase64 :: String -> IO (Either String Text)
encodeMediaToBase64 mediaPath = do
    if  "https://" `isPrefixOf` mediaPath
        then do 
            -- Fetch online media
            -- Create a new HTTP manager
            manager <- newManager tlsManagerSettings

            -- Parse the base URL and get the response
            request <- parseRequest mediaPath
            response <- httpLbs request manager

            let responseStatus' = responseStatus response
            let responseBody' = responseBody response

            case statusCode responseStatus' of
                200 -> do
                    -- Encode the media into base64 format
                    let mediaBytes = responseBody response
                    let mediaBytesB64 = B64.encode (BL.toStrict mediaBytes)
                    return $ Right $ decodeUtf8 mediaBytesB64
                            
                code -> do 
                    -- Decode the error response body into a string
                    let errorDetails = BL.unpack responseBody'
                    let responseStatusMessage' = statusMessage responseStatus'
                    return $ Left $ "error " ++ show code ++ ": " ++ BS.unpack responseStatusMessage' ++ ". " ++ errorDetails

        else do
            -- Check whether the local media exists
            fileExists <- doesFileExist mediaPath

            if not fileExists
                then return $ Left "error: File does not exist."
                else do
                    -- Encode the media into base64 format
                    mediaBytes <- BS.readFile mediaPath 
                    let mediaBytesB64 = B64.encode mediaBytes
                    return $ Right $ decodeUtf8 mediaBytesB64
        

defaultIOMediaChatRequest :: String -> String -> IO (Either String ChatRequest)
defaultIOMediaChatRequest mediaPath message = do
    let mediaType' = getMediaType mediaPath
    let msgType' = if mediaType' == "application/pdf" then "document" else "image"
    encodedMediaResult <- encodeMediaToBase64 mediaPath
    case encodedMediaResult of 
        Left err -> return $ Left err
        Right encodedMedia -> do
            let mediaSource = MediaSource 
                    { encodingType = "base64"
                    , mediaType = mediaType'
                    , imageData = encodedMedia
                    }
            return $ Right ChatRequest 
                { model = defaultModel
                , messages = 
                    [ RequestMessage
                        { role = "user"
                        , content = 
                            Right
                                [ MediaContent { msgType = msgType', source = mediaSource }
                                , TextContent { msgType = "text", text = message }
                                ]
                        }
                    ]
                , maxTokens = 1024
                , stopSequences = Nothing
                , stream = Nothing
                , system = Nothing
                , temperature = Nothing
                }


addMediaToChatRequest :: String -> String -> ChatRequest -> IO (Either String ChatRequest)
addMediaToChatRequest mediaPath message req = do
    imageRequest <- defaultIOMediaChatRequest mediaPath message
    case imageRequest of
        Left err -> return $ Left err
        Right newImageRequest -> return $ Right req { messages = messages req ++ messages newImageRequest }


chatBot :: IO ()
chatBot = do
    putStrLn "Enter your first message (or type 'QUIT' to exit)\n"
    let req = defaultChatRequest "Hi Claude."
    chatHelper req

    where 
        chatHelper :: ChatRequest -> IO ()
        chatHelper chatReq = do
            -- Send the initial request to Claude
            resp <- chat chatReq

            case resp of 
                Left err -> putStrLn err
                Right chatResp -> do
                    -- Add Claude's response into the new request
                    let respContent =
                            responseText $ head $ responseContent chatResp

                    let updatedChatReq = 
                            addResponseToChatRequest chatReq chatResp

                    putStrLn $ "Claude:\n-------\n" ++ respContent ++ "\n"

                    putStrLn "You:\n----"  
                    userReply <- getLine
                    putStrLn ""

                    case userReply of 
                        "CLEAR" -> do
                            -- Empty chat history
                            putStrLn "You:\n----"  
                            newMessage <- getLine
                            putStrLn ""
                            chatHelper $ defaultChatRequest newMessage
                        _ -> do
                            if isMediaRequest userReply
                                then do
                                    -- New media message
                                    -- Get the instructions for the media request.
                                    putStr "Instructions: "  
                                    instructions <- getLine
                                    putStrLn ""

                                    -- Try loading the media into the request
                                    let mediaPath = drop 6 userReply
                                    mediaRequest <- addMediaToChatRequest mediaPath instructions updatedChatReq

                                    case mediaRequest of
                                        Left err -> putStrLn err
                                        Right newChatReq -> chatHelper newChatReq
                                else do 
                                    -- New text message
                                    let newChatReq = 
                                            addMessageToChatRequest "user" userReply updatedChatReq
                                    chatHelper newChatReq

        isMediaRequest :: String -> Bool
        isMediaRequest input = "MEDIA:" `isPrefixOf` input