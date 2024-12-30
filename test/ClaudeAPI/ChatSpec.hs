module ClaudeAPI.ChatSpec where

import ClaudeAPI.Config (defaultModel)
import ClaudeAPI.Chat 
    ( defaultChatRequest
    , defaultCountTokenRequest
    , defaultIOMediaChatRequest
    , sendRequest
    , chat
    , addMessageToChatRequest
    , addResponseToChatRequest
    , countToken
    , getMediaType
    , encodeMediaToBase64
    )
import ClaudeAPI.Types 
    ( ChatRequest (..)
    , ChatResponse (..)
    , RequestMessage (role)
    , ResponseMessage (responseText)
    , RequestMessageContent (..)
    , CountTokenResponse (..)
    )

import Data.List (isInfixOf)
import Test.Hspec


testMediaRequest :: IO (Either String ChatRequest)
testMediaRequest = do
    let mediaPath = "https://www.w3.org/WAI/ER/tests/xhtml/testfiles/resources/pdf/dummy.pdf"
    let message = "What is written in this file?"
    defaultIOMediaChatRequest mediaPath message
    

spec :: Spec
spec = do
    describe "sendRequest" $ do
        it "sends a text message to Anthropic's API successfully" $ do
            let req = defaultChatRequest "Hello Claude"
            results <- sendRequest "POST" "/v1/messages" (Just req) 
                :: IO (Either String ChatResponse)
            case results of
                Left err -> expectationFailure $ "API call failed: " ++ err
                Right resp -> do
                    responseType resp `shouldBe` "message"
                    responseRole resp `shouldBe` "assistant"
                    responseModel resp `shouldBe` defaultModel

    describe "chat" $ do
        it "sends a media file to Anthropic's API successfully" $ do
            eitherMediaReq <- testMediaRequest
            case eitherMediaReq of 
                Left err -> expectationFailure $ "Failed to fetch online document: " ++ err
                Right mediaReq -> do
                    mediaResp <- chat mediaReq
                    case mediaResp of 
                        Left err -> expectationFailure $ "API call failed: " ++ err
                        Right resp -> do
                            responseType resp `shouldBe` "message"
                            responseRole resp `shouldBe` "assistant"
                            responseModel resp `shouldBe` defaultModel
                            responseText  
                               (head $ responseContent resp)
                                    `shouldSatisfy` 
                                        ( \txt -> 
                                            all (`isInfixOf` txt) ["Dummy", "PDF", "file"]
                                        )

    describe "countToken" $ do
        it "counts the tokens of a request successfully" $ do 
            countTokenResp <- countToken $ defaultCountTokenRequest "Hello Claude"
            case countTokenResp of 
                Left err -> expectationFailure $ "API call failed: " ++ err
                Right (CountTokenResponse inputTokens') -> do
                    inputTokens' `shouldSatisfy` (> 0)

    describe "addMessageToChatRequest" $ do
        it "add a message string to the ChatRequest object successfully" $ do
            let originalReq = defaultChatRequest "Hello world"
            let newReq = addMessageToChatRequest "user" "Hi" originalReq
            model newReq `shouldBe` defaultModel
            let messages' = messages newReq
            length messages' `shouldBe` 2
    
    describe "addResponseToChatRequest" $ do
        it "add a message from ChatResponse to the ChatRequest object successfully" $ do
            let originalReq = defaultChatRequest "Hello world"
            chatResp <- chat originalReq
            case chatResp of 
                    Left err -> expectationFailure $ "Failed to get a response message: " ++ err
                    Right resp -> do
                        let newReq = addResponseToChatRequest originalReq resp
                        model newReq `shouldBe` defaultModel
                        let messages' = messages newReq
                        length messages' `shouldBe` 2
                        role (last messages') `shouldBe` "assistant"

    describe "getMediaType" $ do 
        it "converts file paths into mime strings successfully" $ do
            getMediaType "docs.pdf" `shouldBe` "application/pdf"
            getMediaType "image.jpg" `shouldBe` "image/jpeg"
            getMediaType "image.png" `shouldBe` "image/png"
                                    
    describe "encodeMediaToBase64" $ do 
        it "encodes a media file into base64 format successfully" $ do
            mediaBytes <- encodeMediaToBase64 
                "https://www.w3.org/WAI/ER/tests/xhtml/testfiles/resources/pdf/dummy.pdf"
            case mediaBytes of 
                Left err -> expectationFailure $ "Failed to encode the media file: " ++ err
                Right _ -> pure ()
