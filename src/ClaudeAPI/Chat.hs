{-# LANGUAGE DuplicateRecordFields #-}

module ClaudeAPI.Chat where

import ClaudeAPI.Types
    ( ChatResponse(..),
      ChatRequest(..),
      RequestMessage(..),
      RequestMessageContent(..),
      ResponseMessage(..),
      CountTokenRequest(..),
      CountTokenResponse(..),
      ImageSource (..),
    )
import ClaudeAPI.Config ( anthropicVersion, baseUrl )

import Configuration.Dotenv (loadFile, defaultConfig)  
import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
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
    , max_tokens = 1024
    , stop_sequences = Nothing
    , stream = Nothing
    , system = Nothing
    , temperature = Nothing
    }


-- Send any request to the Claude API
sendRequest
    :: (ToJSON req, FromJSON resp) 
    => String -- request method
    -> String -- endpoint
    -> Maybe req 
    -> IO (Either String resp)
sendRequest requestMethod endpoint chatReq  = do
    -- Try to load the API key from the .env file
    loadFile defaultConfig
    getApiKey <- try (getEnv "API_KEY") :: IO (Either SomeException String)

    case getApiKey of 
        Left _ -> return $ Left "error: API key not found"
        Right apiKey -> do
            -- Create a new HTTP manager
            manager <- newManager tlsManagerSettings

            -- Parse the base URL
            initialRequest <- parseRequest $ baseUrl ++ endpoint

            -- Encode the request as JSON
            let claudeRequestBody = maybe mempty (RequestBodyLBS . encode) chatReq
            putStrLn $ BL.unpack $ encode chatReq

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
                200 -> do
                    -- Decode the response body into an object
                    let mayResponseBody = decode responseBody'

                    case mayResponseBody of
                        Just responseBodyObject -> return $ Right responseBodyObject
                        Nothing -> return $ Left "error: failed to decode response body"
                    
                code -> do 
                    -- Decode the error response body into a string
                    let errorDetails = BL.unpack responseBody'
                    let responseStatusMessage' = statusMessage responseStatus'
                    return $ Left $ "error " ++ show code ++ ": " ++ BS.unpack responseStatusMessage' ++ ". " ++ errorDetails


chat :: ChatRequest -> IO (Either String ChatResponse)
chat req = sendRequest "POST" "/v1/messages" (Just req)
 

addMessageToChatRequest :: String -> String -> ChatRequest -> ChatRequest
addMessageToChatRequest r m req = 
    ChatRequest {
        model = model req,
        messages = 
            messages req ++
            [ RequestMessage { role = r, content = Left m } ],
        max_tokens = max_tokens req,
        stop_sequences = stop_sequences req,
        stream = stream req,
        system = system req,
        temperature = temperature req
        }


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
                        "QUIT" -> putStrLn "ESC pressed. Bye :)"
                        _ -> do
                            let newChatReq = 
                                    addMessageToChatRequest "user" userReply updatedChatReq
                            chatHelper newChatReq

    
defaultCountTokenRequest :: String -> CountTokenRequest
defaultCountTokenRequest reqContent = CountTokenRequest 
    { countTokenModel = "claude-3-5-sonnet-20241022"
    , countTokenMessages = [RequestMessage { role = "user", content = Left reqContent }]
    , countTokenSystem = Nothing
    }

    
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)
countToken req = sendRequest "POST" "/v1/messages/count_tokens" (Just req)


getMediaType :: FilePath -> String
getMediaType mediaPath =
    case map toLower (takeExtension mediaPath) of 
        ".jpg" -> "image/jpeg"
        other -> "image/" ++ drop 1 other


encodeImageToBase64 :: FilePath -> IO Text
encodeImageToBase64 imagePath = do
    fileExists <- doesFileExist imagePath
    if not fileExists
        then return $ pack "Error: File does not exist."
        else do
            imageBytes <- BS.readFile imagePath 
            let imageBytesB64 = B64.encode imageBytes
            return $ decodeUtf8 imageBytesB64
    

defaultImageChatRequest :: String -> String -> IO ChatRequest
defaultImageChatRequest imagePath message = do
    imageSource <- ioImageSource
    return ChatRequest 
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
        , max_tokens = 1024
        , stop_sequences = Nothing
        , stream = Nothing
        , system = Nothing
        , temperature = Nothing
        }
    where 
        ioImageSource = do
            encodedImage <- encodeImageToBase64 imagePath
            return ImageSource 
                { encodingType = "base64"
                , mediaType = getMediaType imagePath
                , imageData = encodedImage
                }