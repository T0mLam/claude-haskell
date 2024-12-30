
module ClaudeAPI.MessageBatchesSpec where
    
import ClaudeAPI.Chat (defaultChatRequest)
import ClaudeAPI.MessageBatches
    ( createMessageBatch
    , retrieveMessageBatch
    , listMessageBatch
    , cancelMessageBatch
    -- retrieveMessageBatchResults cannot be tested as it requires time to process.
    )
import ClaudeAPI.Types
    ( MessageBatchRequest(..)
    , MessageBatchRequests(..)
    , MessageBatchResponse (..)
    , ListMessageBatchRequest (..)
    , ListMessageBatchResponse (..)
    )

import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Test.Hspec
import GHC.IO (unsafePerformIO)


testMessageBatchRequests :: MessageBatchRequests
testMessageBatchRequests = MessageBatchRequests
    { requests =
        [ MessageBatchRequest
            { customID = "test1"
            , params = defaultChatRequest "Hello World"
            }
        ]
    }

-- Store the message batch id from createMessageBatch response
msgBatchIDRef :: IORef String
{-# NOINLINE msgBatchIDRef #-}
msgBatchIDRef = unsafePerformIO $ newIORef ""

spec :: Spec
spec = do
    describe "createMessageBatch" $ do
        it "creates a new message batch successfully" $ do
            msgBatchResp <- createMessageBatch testMessageBatchRequests
            case msgBatchResp of
                Left err -> expectationFailure $ "Failed to create a new message batch: " ++ err
                Right (MessageBatchResponse id' responseType' _ _ _ _ _ _ _ _) -> do
                    id' `shouldStartWith` "msgbatch_"
                    responseType' `shouldBe` "message_batch"
                    writeIORef msgBatchIDRef id'

    describe "retrieveMessageBatch" $ do
        it "fetches the newly created message batch successfully" $ do
            msgBatchID <- readIORef msgBatchIDRef
            if null msgBatchID
                then expectationFailure "Failed to create a message batch in the previous test."
                else do
                    msgBatchResp <- retrieveMessageBatch msgBatchID
                    case msgBatchResp of
                        Left err -> expectationFailure $ "Failed to retrieve message batch: " ++ err
                        Right (MessageBatchResponse id' responseType' processingStatus' _ _ _ _ _ _ _) -> do
                            id' `shouldStartWith` "msgbatch_"
                            responseType' `shouldBe` "message_batch"
                            processingStatus' `shouldSatisfy` (`elem` ["in_progress", "ended"])
    
    describe "listMessageBatch" $ do
        it "lists all created message batches successfully" $ do
            msgBatchResps <- listMessageBatch 
                (ListMessageBatchRequest { beforeID = Nothing, limit = Nothing, afterID = Nothing }) 
            case msgBatchResps of
                    Left err -> expectationFailure $ "Failed to list all message batches: " ++ err
                    Right (ListMessageBatchResponse resps) -> do
                        length resps `shouldSatisfy` (> 0)
                        let (MessageBatchResponse id' responseType' processingStatus' _ _ _ _ _ _ _) = head resps
                        id' `shouldStartWith` "msgbatch_"
                        responseType' `shouldBe` "message_batch"
                        processingStatus' `shouldSatisfy` (`elem` ["in_progress", "ended"])

    describe "cancelMessageBatch" $ do
        it "cancels the created message batch successfully" $ do
            msgBatchID <- readIORef msgBatchIDRef
            if null msgBatchID
                then expectationFailure "Failed to create a message batch in the previous test."
                else do
                    msgBatchResp <- cancelMessageBatch msgBatchID
                    case msgBatchResp of
                        Left err -> expectationFailure $ "Failed to cancel message batch: " ++ err
                        Right (MessageBatchResponse id' responseType' processingStatus' _ _ _ _ _ _ _) -> do
                            id' `shouldStartWith` "msgbatch_"
                            responseType' `shouldBe` "message_batch"
                            processingStatus' `shouldBe` "canceling"