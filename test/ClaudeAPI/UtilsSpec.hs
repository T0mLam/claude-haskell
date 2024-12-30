module ClaudeAPI.UtilsSpec where

import ClaudeAPI.Models (defaultModelRequest)
import ClaudeAPI.MessageBatches ()
import ClaudeAPI.Types (ListMessageBatchRequest (..))
import ClaudeAPI.Utils (buildQueryString, camelToUnderscore)

import Test.Hspec

spec :: Spec 
spec = do 
    describe "buildQueryString" $ do
        it "builds a query string from ModelRequest successfully" $ do
            buildQueryString defaultModelRequest `shouldBe` ""
        it "builds a query string from ListMessageBatchRequest successfully" $ do 
            buildQueryString 
                (ListMessageBatchRequest { beforeID = Just "no", limit = Just 2, afterID = Nothing }) 
                `shouldBe` "?before_id=no&limit=2"

    describe "camelToUnderscore" $ do
        it "converts the string 'beforeId' into 'before_id'" $ do
            camelToUnderscore "beforeId" `shouldBe` "before_id"
        it "converts the string 'id' into 'id'" $ do
            camelToUnderscore "id" `shouldBe` "id"