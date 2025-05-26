module ClaudeAPI.ModelsSpec where

import ClaudeAPI.Config (defaultModel)
import ClaudeAPI.Models (listModels, defaultModelRequest, getModel)
import ClaudeAPI.Types (ModelRequest (..), ModelResponse (..), ModelData (..))

import Test.Hspec

spec :: Spec
spec = do 
    describe "listModels" $ do
        it "fetches a list of models successfully" $ do
            results <- listModels $ defaultModelRequest { limit = Just 2 }
            case results of 
                Left err -> expectationFailure $ "API call failed: " ++ err
                Right (ModelResponse modelData' hasMore' _ _) -> do
                    length modelData' `shouldBe` 2
                    hasMore' `shouldBe` True
    
    describe "getModel" $ do
        it "fetches the details of a model successfully" $ do
            modelDetails <- getModel defaultModel
            case modelDetails of 
                Left err -> expectationFailure $ "API call failed: " ++ err
                Right (ModelData modelType' modelID' displayName' _) -> do
                    modelType' `shouldBe` "model"
                    modelID' `shouldBe` defaultModel 
                    displayName' `shouldBe` "Claude Sonnet 3.5 (New)"