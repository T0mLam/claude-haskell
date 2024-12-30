module Main (main) where

import Test.Hspec

import qualified ClaudeAPI.ChatSpec
import qualified ClaudeAPI.MessageBatchesSpec
import qualified ClaudeAPI.ModelsSpec
import qualified ClaudeAPI.UtilsSpec

main :: IO ()
main = hspec $ do
    describe "ClaudeAPI.Chat" ClaudeAPI.ChatSpec.spec
    describe "ClaudeAPI.MessageBatches" ClaudeAPI.MessageBatchesSpec.spec
    describe "ClaudeAPI.Models" ClaudeAPI.ModelsSpec.spec
    describe "ClaudeAPI.Utils" ClaudeAPI.UtilsSpec.spec
