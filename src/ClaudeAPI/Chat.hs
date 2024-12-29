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
    , ImageSource (..)
    , JSONResponse (..)
    )
import ClaudeAPI.Config (baseUrl)

import Configuration.Dotenv (loadFile, defaultConfig)  
import Control.Exception (SomeException, try)
import Data.Aeson (encode, ToJSON)
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
    { model = "claude-3-5-sonnet-20241022"
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
    loadFile defaultConfig
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


chatBot :: IO ()
chatBot = do
    putStrLn "Enter your first message (or type 'QUIT' to exit)\n"
    let req = defaultChatRequest "Hi Claude."
    chatHelper req

    where 
        chatHelper :: ChatRequest -> IO ()
        chatHelper chatReq = do
            resp <- chat chatReq

            case resp of 
                Left _ -> pure ()
                Right chatResp -> do
                    let respContent =
                            responseText $ head $ responseContent chatResp

                    let updatedChatReq = 
                            addResponseToChatRequest chatReq chatResp

                    putStrLn $ "Claude:\n-------\n" ++ respContent ++ "\n"

                    -- forgot to ask user for content
                    putStrLn "You:\n----"  
                    userReply <- getLine
                    putStrLn ""

                    case userReply of 
                        "QUIT" -> return ()
                        _ -> do
                            let newChatReq = 
                                    addMessageToChatRequest "user" userReply updatedChatReq
                            chatHelper newChatReq

    
defaultCountTokenRequest :: String -> CountTokenRequest
defaultCountTokenRequest reqContent = CountTokenRequest 
    { model = "claude-3-5-sonnet-20241022"
    , requestMessages = [RequestMessage { role = "user", content = Left reqContent }]
    , system = Nothing
    }

    
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)
countToken req = sendRequest "POST" "/v1/messages/count_tokens" (Just req)


getMediaType :: FilePath -> String
getMediaType mediaPath =
    case map toLower (takeExtension mediaPath) of 
        ".jpg" -> "image/jpeg"
        other -> "image/" ++ drop 1 other


encodeImageToBase64 :: String -> IO (Either String Text)
encodeImageToBase64 imagePath = do
    if  "https://" `isPrefixOf` imagePath
        then do 
            -- fetch online image
            -- Create a new HTTP manager
            manager <- newManager tlsManagerSettings

            -- Parse the base URL and get the response e
            request <- parseRequest imagePath
            response <- httpLbs request manager

            let responseStatus' = responseStatus response
            let responseBody' = responseBody response

            case statusCode responseStatus' of
                200 -> do
                    -- Encode the image into base64 format
                    let imageBytes = responseBody response
                    let imageBytesB64 = B64.encode (BL.toStrict imageBytes)
                    return $ Right $ decodeUtf8 imageBytesB64
                            
                code -> do 
                    -- Decode the error response body into a string
                    let errorDetails = BL.unpack responseBody'
                    let responseStatusMessage' = statusMessage responseStatus'
                    return $ Left $ "error " ++ show code ++ ": " ++ BS.unpack responseStatusMessage' ++ ". " ++ errorDetails

        else do
            -- Vheck whether the local image exists
            fileExists <- doesFileExist imagePath

            if not fileExists
                then return $ Left "error: File does not exist."
                else do
                    -- Encode the image into base64 format
                    imageBytes <- BS.readFile imagePath 
                    let imageBytesB64 = B64.encode imageBytes
                    return $ Right $ decodeUtf8 imageBytesB64
        

defaultIOImageChatRequest :: String -> String -> IO (Either String ChatRequest)
defaultIOImageChatRequest imagePath message = do
    encodedImageResult <- encodeImageToBase64 imagePath
    case encodedImageResult of 
        Left err -> return $ Left err
        Right encodedImage -> do
            let imageSource = ImageSource 
                    { encodingType = "base64"
                    , mediaType = getMediaType imagePath
                    , imageData = encodedImage
                    }
            return $ Right ChatRequest 
                { model = "claude-3-5-sonnet-20241022"
                , messages = 
                    [ RequestMessage
                        { role = "user"
                        , content = 
                            Right
                                [ ImageContent { msgType = "image", source = imageSource }
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


test :: IO (Either String ChatResponse)
test = do
    let imagePath = "https://thumbs.dreamstime.com/b/red-apple-isolated-clipping-path-19130134.jpg"
    let message = "What is in this image?"
    result <- defaultIOImageChatRequest imagePath message
    case result of
        Left err -> return $ Left err
        Right chatRequest -> chat chatRequest