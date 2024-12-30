module Main (main) where

import Test.Hspec

import qualified ClaudeAPI.ChatSpec
import qualified ClaudeAPI.MessageBatchesSpec
import qualified ClaudeAPI.ModelsSpec
import qualified ClaudeAPI.TypesSpec
import qualified ClaudeAPI.UtilsSpec

main :: IO ()
main = hspec $ do
    describe "ClaudeAPI.MessageBatchesSpec" ClaudeAPI.MessageBatchesSpec.spec
    describe "ClaudeAPI.Models" ClaudeAPI.ModelsSpec.spec
    describe "ClaudeAPI.Utils" ClaudeAPI.UtilsSpec.spec
