#  claude-haskell 

[![License: MIT](https://cdn.prod.website-files.com/5e0f1144930a8bc8aace526c/65dd9eb5aaca434fac4f1c34_License-MIT-blue.svg)](/LICENSE)
[![GitHub release](https://img.shields.io/github/release/T0mLam/claude-haskell.svg)](https://github.com/T0mLam/claude-haskell/releases/)

**claude-haskell is an unofficial binding for Anthropic's Claude API. This project has not been reviewed or published as an official package.**

This library provides Haskell functions to interact with the Claude API, including sending text messages, images, pdf documents, listing and retrieving model information, and create message batches. It includes utilities and types for building API requests and handling responses.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
    - [Send messages](#send-messages)
    - [Use a pre-defined chatbot](#use-a-pre-defined-chatbot)
    - [Count the number of tokens in a message](#count-the-number-of-tokens-in-a-message)
    - [List all available models](#list-all-available-models)
    - [Get model details](#get-model-details)
    - [Message batch operations](#message-batch-operations)
    - [Create custom requests to Anthropic's API](#create-custom-requests-to-anthropics-api)  
- [Modules](#modules)
    - [**ClaudeAPI.Config**](#claudeapiconfig)
        - [baseUrl](#baseurl)
        - [defaultModel](#defaultmodel)
    - [**ClaudeAPI.Chat**](#claudeapichat)
        - [defaultChatRequest](#defaultchatrequest)
        - [sendRequest](#sendrequest)
        - [chat](#chat)
        - [addMessageToChatRequest](#addmessagetochatrequest)
        - [addResponseToChatRequest](#addresponsetochatrequest)
        - [defaultCountTokenRequest](#defaultcounttokenrequest)
        - [countToken](#counttoken)
        - [encodeMediaToBase64](#encodemediatobase64)
        - [defaultIOMediaChatRequest](#defaultiomediachatrequest)
        - [addMediaToChatRequest](#addmediatochatrequest)
        - [chatBot](#chatbot)
    - [**ClaudeAPI.MessageBatches**](#claudeapimessagebatches)
        - [createMessageBatch](#createmessagebatch)
        - [retrieveMessageBatch](#retrievemessagebatch)
        - [listMessageBatch](#listmessagebatch)
        - [cancelMessageBatch](#cancelmessagebatch)
        - [retrieveMessageBatchResults](#retrievemessagebatchresults)
    - [**ClaudeAPI.Models**](#claudeapimodels)
        - [defaultModelRequest](#defaultmodelrequest)
        - [listModels](#listmodels)
        - [getModel](#getmodel)
    - [**ClaudeAPI.Types**](#claudeapitypes)
        - [JSONResponse: parseResponse](#typeclass-jsonresponse)
        - [MediaSource](#mediasource)
        - [RequestMessageContent](#requestmessagecontent)
        - [RequestMessage](#requestmessage)
        - [ChatRequest](#chatrequest)
        - [ResponseMessage](#responsemessage)
        - [ChatResponse](#chatresponse)
        - [Usage](#usage)
        - [ModelRequest](#modelrequest)
        - [ModelResponse](#modelresponse)
        - [CountTokenRequest](#counttokenrequest)
        - [CountTokenResponse](#counttokenresponse)
        - [MessageBatchRequest](#messagebatchrequest)
        - [MessageBatchRequests](#messagebatchrequests)
        - [MessageBatchRequestCount](#messagebatchrequestcount)
        - [MessageBatchResponse](#messagebatchresponse)
        - [ListMessageBatchRequest](#listmessagebatchrequest)
        - [ListMessageBatchResponse](#listmessagebatchresponse)
        - [MessageBatchResult](#messagebatchresult)
        - [RetrieveMessageBatchResult](#retrievemessagebatchresult)
        - [RetrieveMessageBatchResults](#retrievemessagebatchresults-1)
    - [**ClaudeAPI.Utils**](#claudeapiutils)
        - [buildQueryString](#buildquerystring)
        - [camelToUnderscore](#cameltounderscore)
- [Testing](#testing)
- [Contributing](#contributing)
- [License](#license)

---

## Installation

1. Create a new file `cabal.project` in the root directory of your cabal project
   ```bash
   new-project
   ├── CHANGELOG.md
   ├── LICENSE
   ├── app
   │   └── Main.hs
   ├── cabal.project
   └── new-project.cabal
   ```

2. Add claude-haskell as a package in `cabal.project`:
   ```bash
   source-repository-package
    type: git
    location: https://github.com/T0mLam/claude-haskell.git

   packages: ./<filename>.cabal
   ```

3. Update the `.cabal` configurations
   ```
   -- Other library packages from which modules are imported.
   build-depends:    base ^>=4.17.2.1,
                     claude-haskell
   ```

5. Set the API key and Anthropic version in the environment (`.env`) file:
   ```bash
   new-project
   ├── CHANGELOG.md
   ├── LICENSE
   ├── app
   │   └── Main.hs
   ├── cabal.project
   ├── .env # Create a new environment file 
   └── new-project.cabal
   ```
   `.env`

   ```bash
   API_KEY=<api_key>
   ANTHROPIC_VERSION=<anthropic_version> # e.g. 2023-06-01
   ``` 

## Usage

Below is a guide to the key functionalities and corresponding functions.

### Send messages

[`chat`](#chat) [`defaultChatRequest`](#defaultchatrequest)   [`defaultIOImageChatRequest`](#defaultioimagechatrequest) [`ChatRequest`](#chatrequest)

```haskell
chat :: ChatRequest -> IO (Either String ChatResponse)
```

```haskell
defaultChatRequest
    :: String -- Text message
    -> ChatRequest
```

```haskell
defaultIOMediaChatRequest 
    :: String -- File path (i.e. local, URL)
    -> String -- Instructions
    -> IO (Either String ChatRequest)
```

```haskell
data ChatRequest = ChatRequest
    { model :: String
    , messages :: [RequestMessage]
    , max_tokens :: Int
    , stop_sequences :: Maybe String
    , stream :: Maybe Bool
    , system :: Maybe String
    , temperature :: Maybe Double
    }
```

**Example:**

Send text messages

```haskell
main :: IO ()
main = do
    -- Make a chat request to Claude
    chatResponse <- chat $ defaultChatRequest "Where is the capital of China?"

    case chatResponse of
        Left err -> putStrLn $ "Error: " ++ err
        Right resp -> do
            -- Extract the message from the ChatResponse object
            let botReply = responseText $ head $ responseContent resp
            putStrLn $ "Bot: " ++ botReply
```

Send images or PDF documents

```haskell
main :: IO ()
main = do
    let imagePath = "https://thumbs.dreamstime.com/b/red-apple-isolated-clipping-path-19130134.jpg"
    let instructions = "What is in this image?"

    -- Encode the image into base64 format 
    result <- defaultIOImageChatRequest imagePath instructions

    case result of
        Left err -> return $ Left err
        Right chatRequest -> do
            -- Make a chat request to Claude
            chatReponse <- chat chatRequest

            case chatResponse of
                Left err -> putStrLn $ "Error: " ++ err
                Right resp -> do
                    -- Extract the message from the ChatResponse object
                    let botReply = responseText $ head $ responseContent resp
                    putStrLn $ "Bot: " ++ botReply
```

</br>

### Use a pre-defined chatbot

[`chatBot`](#chatbot)

```haskell
chatBot :: IO ()
```

**Example:**

```haskell
main :: IO ()
main = chatBot
```

</br>

### Count the number of tokens in a message

[`countToken`](#counttoken) [`defaultCountTokenRequest`](#defaultcounttokenrequest) [`CountTokenRequest`](#counttokenrequest-and-counttokenresponse)

```haskell
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)
```

```haskell
defaultCountTokenRequest :: String -> CountTokenRequest
```

```haskell
data CountTokenRequest = CountTokenRequest
    { requestMessages :: [RequestMessage]
    , model :: String
    , system :: Maybe String
    }
```

**Example:**

```haskell
main :: IO ()
main = do
    countTokenResp <- countToken $ defaultCountTokenRequest "Hello Claude"
    case countTokenResp of
        Left err -> putStrLn $ "Error: " ++ err
        Right resp -> do
            let tokenCount = inputTokens resp
            putStrLn $ "Token count: " ++ tokenCount
```

**Notes:**
Create custom `CountTokenRequest` for counting tokens for media requests.

</br>

### List all available models

[`listModels`](#listmodels) [`defaultModelRequest`](#defaultmodelrequest)

```haskell
listModels :: ModelRequest -> IO (Either String ModelResponse)
```

```haskell
defaultModelRequest :: ModelRequest
```

</br>

### Get model details

[`getModel`](#getmodel)

```haskell
getModel :: String -> IO (Either String ModelData)
```

</br>

### Message batch operations

[`createMessageBatch`](#createmessagebatch) [`retrieveMessageBatch`](#retrievemessagebatch) [`listMessageBatch`](#listmessagebatch) [`cancelMessageBatch`](#cancelmessagebatch) [`retrieveMessageBatchResults`](#retrievemessagebatchresults)


```haskell
createMessageBatch 
    :: MessageBatchRequests 
    -> IO (Either String MessageBatchResponse)
```

```haskell
retrieveMessageBatch 
    :: String -- Message batch ID, e.g. msg_...
    -> IO (Either String MessageBatchResponse)
```

```haskell
listMessageBatch 
    :: ListMessageBatchRequest 
    -> IO (Either String ListMessageBatchResponse)
```

```haskell
cancelMessageBatch 
    :: String -- Message batch ID, e.g. msg_...
    -> IO (Either String MessageBatchResponse)
```

```haskell
retrieveMessageBatchResults 
    :: String -- Message batch ID, e.g. msg_...
    -> IO (Either String RetrieveMessageBatchResults)
```

</br>

### Create custom requests to Anthropic's API

[`sendRequest`](#sendrequest)

```haskell
sendRequest
    :: (ToJSON req, JSONResponse resp) 
    => String -- request method
    -> String -- endpoint
    -> Maybe req 
    -> IO (Either String resp)
```

**Example:**

```haskell
data ExampleReq = ExampleReq {...} 
    deriving (Show, Generic, ToJson)

-- JSONResponse is a subclass of FromJSON,
-- which determines how the content is decoded 
-- to the data object (default=Data.Aeson.eitherDecode) 
data ExampleResp = ExampleResp {...} 
    deriving (Show, Generic, JSONResponse)

exampleFunc :: ExampleReq -> IO (Either String ExampleResp)
exampleFunc req = sendRequest "POST" "/v1/messages" (Just req)
```

---

## Modules

### ClaudeAPI.Config

This module defines configuration constants for interacting with the Claude API, including the base URL and default model.

#### `baseUrl`

The base URL for the Claude API.

```haskell
baseUrl :: String
```

- **Description:** Specifies the root endpoint for the API.
- **Value:** `"https://api.anthropic.com"`

</br>

#### `defaultModel`

The default model to be used for requests to the Claude API.

```haskell
defaultModel :: String
```

- **Description:** Specifies the default Claude model version for API calls.
- **Value:** `"claude-3-5-sonnet-20241022"`

</br>

### ClaudeAPI.Chat

This module provides functionality for interacting with the Claude API to send messages, handle chat requests, and manage image-based requests.

#### `defaultChatRequest`

Creates a default `ChatRequest` with a specified message from the user. The model used is `claude-3-5-sonnet-20241022`.

**Type:**
```haskell
defaultChatRequest :: String -> ChatRequest
```

**Example:**
```haskell
let req = defaultChatRequest "Hi Claude."
```
</br>

#### `sendRequest`

Sends a request to the Claude API using the specified HTTP method, endpoint, and request body.

**Type:**
```haskell
sendRequest :: (ToJSON req, JSONResponse resp) => String -> String -> Maybe req -> IO (Either String resp)
```

**Parameters:**
- `requestMethod`: The HTTP method (e.g., "POST").
- `endpoint`: The API endpoint (e.g., "/v1/messages").
- `chatReq`: The request body, encoded as JSON.

**Example:**
```haskell
resp <- sendRequest "POST" "/v1/messages" (Just myChatRequest)
```
</br>

#### `chat`

Sends a chat request to the Claude API and returns a `ChatResponse`.

**Type:**
```haskell
chat :: ChatRequest -> IO (Either String ChatResponse)
```

**Example:**
```haskell
let req = defaultChatRequest "Hello, Claude!"
resp <- chat req
```
</br>

#### `addMessageToChatRequest`

Adds a new message to an existing `ChatRequest`.

**Type:**
```haskell
addMessageToChatRequest :: String -> String -> ChatRequest -> ChatRequest
```

**Parameters:**
- `r`: Role of the message sender (e.g., "user").
- `m`: The message content.
- `req`: The existing `ChatRequest`.

**Example:**
```haskell
let updatedReq = addMessageToChatRequest "user" "Another message" req
```
</br>

#### `addResponseToChatRequest`

Adds a response message to an existing `ChatRequest`.

**Type:**
```haskell
addResponseToChatRequest :: ChatRequest -> ChatResponse -> ChatRequest
```

**Parameters:**
- `req`: The existing `ChatRequest`.
- `resp`: The `ChatResponse` to extract the response message from.

**Example:**
```haskell
let updatedReq = addResponseToChatRequest req chatResp
```
</br>

#### `defaultCountTokenRequest`

Creates a default `CountTokenRequest` to estimate the token count for a given input.

**Type:**
```haskell
defaultCountTokenRequest :: String -> CountTokenRequest
```

**Example:**
```haskell
let tokenReq = defaultCountTokenRequest "Estimate token count for this text."
```
</br>

#### `countToken`

Sends a token count request to the Claude API.

**Type:**
```haskell
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)
```

**Example:**
```haskell
let req = defaultCountTokenRequest "Test string for token count."
resp <- countToken req
```
</br>

#### `encodeMediaToBase64`

Encodes a media file (local or remote) to a Base64 string for use in requests.

**Type:**
```haskell
encodeMediaToBase64 :: String -> IO (Either String Text)
```

**Parameters:**
- `mediaPath`: Path to the media file or URL.

**Example:**
```haskell
encodedMedia <- encodeMediaToBase64 "path/to/image.jpg"
```
</br>

#### `defaultIOMediaChatRequest`

Creates a default `ChatRequest` for media files (e.g., images or documents).

**Type:**
```haskell
defaultIOMediaChatRequest :: String -> String -> IO (Either String ChatRequest)
```

**Parameters:**
- `mediaPath`: Path to the media file.
- `message`: Associated text message.

**Example:**
```haskell
req <- defaultIOMediaChatRequest "path/to/image.jpg" "Describe this image."
```
</br>

#### `addMediaToChatRequest`

Adds a media file and its associated message to an existing `ChatRequest`.

**Type:**
```haskell
addMediaToChatRequest :: String -> String -> ChatRequest -> IO (Either String ChatRequest)
```

**Parameters:**
- `mediaPath`: Path to the media file.
- `message`: Associated text message.
- `req`: The existing `ChatRequest`.

**Example:**
```haskell
let req = defaultChatRequest "Initial message."
updatedReq <- addMediaToChatRequest "path/to/image.jpg" "Analyze this image" req
```
</br>

#### `chatBot`

A simple interactive chatbot that communicates with the Claude API. It supports media uploads and maintains chat history.

**Type:**
```haskell
chatBot :: IO ()
```

**Usage:**
Run the function in the terminal to start the chatbot.

**Example:**
```haskell
main :: IO ()
main = chatBot
```

</br>

### ClaudeAPI.MessageBatches

This module provides functionality to manage message batches in the Claude API, including creating, retrieving, listing, and canceling message batches, as well as retrieving their results.

#### `createMessageBatch`

Creates a new message batch by sending a `MessageBatchRequests` object to the Claude API.

**Type:**
```haskell
createMessageBatch :: MessageBatchRequests -> IO (Either String MessageBatchResponse)
```

**Example:**
```haskell
let batchRequest = MessageBatchRequests { ... }
response <- createMessageBatch batchRequest
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right result -> print result
```

</br>

#### `retrieveMessageBatch`

Retrieves details of a specific message batch by its ID.

**Type:**
```haskell
retrieveMessageBatch :: String -> IO (Either String MessageBatchResponse)
```

**Example:**
```haskell
response <- retrieveMessageBatch "batch-id-1234"
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right batch -> print batch
```

</br>

#### `listMessageBatch`

Lists message batches based on the provided filtering and pagination parameters encapsulated in a `ListMessageBatchRequest`.

**Type:**
```haskell
listMessageBatch :: ListMessageBatchRequest -> IO (Either String ListMessageBatchResponse)
```

**Example:**
```haskell
let listRequest = ListMessageBatchRequest { beforeID = Just "batch-id-5678", afterID = Nothing, limit = Just 10 }
response <- listMessageBatch listRequest
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right batches -> print batches
```

</br>

#### `cancelMessageBatch`

Cancels a specific message batch by its ID.

**Type:**
```haskell
cancelMessageBatch :: String -> IO (Either String MessageBatchResponse)
```

**Example:**
```haskell
response <- cancelMessageBatch "batch-id-1234"
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right result -> print result
```

</br>

#### `retrieveMessageBatchResults`

Retrieves the results of a specific message batch by its ID.

**Type:**
```haskell
retrieveMessageBatchResults :: String -> IO (Either String RetrieveMessageBatchResults)
```

**Example:**
```haskell
response <- retrieveMessageBatchResults "batch-id-1234"
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right results -> print results
```

</br>

#### Instance: `HasQueryParams ListMessageBatchRequest`

Defines how query parameters for `ListMessageBatchRequest` are handled to build a query string for the API.

**Methods:**
- `getBeforeID`: Retrieves the `beforeID` parameter.
- `getAfterID`: Retrieves the `afterID` parameter.
- `getLimit`: Retrieves the `limit` parameter.

**Usage:**
```haskell
instance HasQueryParams ListMessageBatchRequest where
    getBeforeID = beforeID
    getAfterID = afterID
    getLimit = limit
```

</br>

### ClaudeAPI.Models

This module provides functionality for interacting with the Claude API to manage and retrieve model information, such as listing available models and retrieving details about a specific model.

#### `defaultModelRequest`

A default `ModelRequest` instance with no filters applied.

**Type:**
```haskell
defaultModelRequest :: ModelRequest
```

**Description:**
- Creates a `ModelRequest` object with default parameters: no `beforeID`, `afterID`, or `limit` specified.

**Example:**
```haskell
let req = defaultModelRequest
```

</br>

#### `listModels`

Retrieves a list of models available via the Claude API.

**Type:**
```haskell
listModels :: ModelRequest -> IO (Either String ModelResponse)
```

**Parameters:**
- `ModelRequest`: A request object specifying optional filters (`beforeID`, `afterID`, and `limit`).

**Returns:**
- Either an error message (`String`) or a `ModelResponse` object containing the list of available models.

**Example:**
```haskell
let req = defaultModelRequest { limit = Just 10 }
response <- listModels req
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right models -> print models
```

</br>

#### `getModel`

Retrieves detailed information about a specific model.

**Type:**
```haskell
getModel :: String -> IO (Either String ModelData)
```

**Parameters:**
- `modelId`: A `String` representing the unique identifier of the model.

**Returns:**
- Either an error message (`String`) or a `ModelData` object containing details about the specified model.

**Example:**
```haskell
response <- getModel "claude-v1"
case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right modelData -> print modelData
```

</br>

#### `HasQueryParams` Instance for `ModelRequest`

Defines the `HasQueryParams` instance for `ModelRequest` to support query string generation for the Claude API.

**Methods:**
- `getBeforeID`: Retrieves the `beforeID` parameter.
- `getAfterID`: Retrieves the `afterID` parameter.
- `getLimit`: Retrieves the `limit` parameter.

**Description:**
This instance allows `ModelRequest` objects to seamlessly generate query strings when interacting with the Claude API.

</br>

### ClaudeAPI.Types

This module defines the types used throughout the ClaudeAPI library. It provides data structures for requests and responses, JSON encoding/decoding, and utilities for interacting with the API.

#### Typeclass: `JSONResponse`

```haskell
class FromJSON a => JSONResponse a where
    parseResponse :: BL.ByteString -> Either String a
```

A typeclass representing types that can be decoded from JSON responses. The default implementation uses `eitherDecode`.

</br>

#### Data Types:

#### `MediaSource`

Represents metadata and data for media (e.g., images).

```haskell
data MediaSource = MediaSource
    { encodingType :: String  -- The encoding type (e.g., "base64").
    , mediaType :: String     -- The MIME type (e.g., "image/png").
    , imageData :: Text       -- The actual image data.
    }
```

- **JSON Serialization:** Converts field names to API-compatible keys (`type`, `media_type`, `data`).

</br>

#### `RequestMessageContent`

Represents the content of a message, which can be either text or media.

```haskell
data RequestMessageContent =
    MediaContent { msgType :: String, source :: MediaSource }
  | TextContent { msgType :: String, text :: String }
```

- **Variants:**
  - `MediaContent`: For messages with media content.
  - `TextContent`: For text messages.
- **JSON Serialization:** Converts `msgType` to `type`.

</br>

#### `RequestMessage`

Represents a request message with a role and content.

```haskell
data RequestMessage = RequestMessage
    { role :: String                    -- The role of the sender (e.g., "user").
    , content :: Either String [RequestMessageContent]  -- Message content.
    }
```

- **JSON Serialization:** Handles either plain string content or structured content.

</br>

#### `ChatRequest`

Represents a chat request containing multiple messages and metadata.

```haskell
data ChatRequest = ChatRequest
    { model :: String                  -- The name of the model to use.
    , messages :: [RequestMessage]     -- List of chat messages.
    , maxTokens :: Int                 -- Maximum number of tokens to generate.
    , stopSequences :: Maybe String    -- Optional stop sequences.
    , stream :: Maybe Bool             -- Whether to stream responses.
    , system :: Maybe String           -- Optional system configurations.
    , temperature :: Maybe Double      -- Temperature parameter for responses.
    }
```

- **JSON Serialization:** Converts camelCase fields to snake_case for API compatibility.

</br>

#### `ResponseMessage`

Represents a response message from the API.

```haskell
data ResponseMessage = ResponseMessage
    { responseType :: String  -- The type of the response (e.g., "text").
    , responseText :: String  -- The text of the response.
    }
```

- **JSON Parsing:** Converts `type` and `text` from JSON.

</br>

#### `ChatResponse`

Represents the full response from the API for a chat request.

```haskell
data ChatResponse = ChatResponse
    { id :: String                     -- Unique identifier for the response.
    , responseType :: String           -- The type of response.
    , responseRole :: String           -- The role of the responder (e.g., "assistant").
    , responseContent :: [ResponseMessage]  -- The content of the response.
    , responseModel :: String          -- The model used to generate the response.
    , stopReason :: String             -- Reason for stopping response generation.
    , stopSequence :: Maybe String     -- Stop sequence, if any.
    , usage :: Usage                   -- Token usage details.
    }
```

- **JSON Parsing:** Maps field names to snake_case for compatibility.

</br>

#### `Usage`

Represents token usage details in a response.

```haskell
data Usage = Usage
    { inputTokens :: Int               -- Number of input tokens.
    , cacheCreationInputTokens :: Int  -- Tokens for cache creation.
    , cacheReadInputTokens :: Int      -- Tokens for cache reading.
    , outputTokens :: Int              -- Number of output tokens.
    }
```

</br>

#### `ModelRequest` and `ModelResponse`

Defines request and response types for retrieving models.

```haskell
data ModelRequest = ModelRequest
    { beforeID :: Maybe String         -- ID for pagination (before).
    , afterID :: Maybe String          -- ID for pagination (after).
    , limit :: Maybe Int               -- Number of results to return.
    }
```

```haskell
data ModelResponse = ModelResponse
    { modelData :: [ModelData]         -- List of models.
    , hasMore :: Bool                  -- Whether there are more models.
    , firstID :: Maybe String          -- First model ID in the response.
    , lastID :: Maybe String           -- Last model ID in the response.
    }
```

</br>

#### `CountTokenRequest` and `CountTokenResponse`

Defines request and response types for counting tokens in a message.

```haskell
data CountTokenRequest = CountTokenRequest
    { requestMessages :: [RequestMessage]
    , model :: String
    , system :: Maybe String
    }
```

```haskell
newtype CountTokenResponse = CountTokenResponse { inputTokens :: Int }
```

</br>

#### `MessageBatchRequest`

Represents a single batch request for sending multiple chat messages.

```haskell
data MessageBatchRequest = MessageBatchRequest
    { customID :: String         -- A custom identifier for the batch request.
    , params :: ChatRequest      -- Parameters for the batch request (e.g., messages, model, etc.).
    }
```

- **Usage:** Allows specifying a custom identifier and the chat request payload for batch operations.
- **JSON Serialization:** Converts `customID` to `custom_id` for API compatibility.

</br>

#### `MessageBatchRequests`

Encapsulates multiple batch requests for submission as a group.

```haskell
newtype MessageBatchRequests =
    MessageBatchRequests { requests :: [MessageBatchRequest] }
```

- **Purpose:** Enables sending multiple `MessageBatchRequest` objects as a single operation.
- **JSON Serialization:** Encodes the list of requests into the required JSON structure.

</br>

#### `MessageBatchRequestCount`

Tracks the processing status of a batch request.

```haskell
data MessageBatchRequestCount = MessageBatchRequestCount
    { processing :: Int   -- Number of requests currently being processed.
    , succeeded :: Int    -- Number of successfully processed requests.
    , errored :: Int      -- Number of requests that resulted in errors.
    , canceled :: Int     -- Number of requests that were canceled.
    , expired :: Int      -- Number of requests that expired before completion.
    }
```

- **Purpose:** Provides detailed counts for different states of batch processing.
- **JSON Parsing:** Automatically maps fields from JSON responses.

</br>

#### `MessageBatchResponse`

Represents the API response for a batch operation.

```haskell
data MessageBatchResponse = MessageBatchResponse
    { id :: String                      -- Unique identifier for the batch.
    , responseType :: String            -- Type of the response (e.g., "batch").
    , processingStatus :: String        -- Current status of the batch (e.g., "completed").
    , requestCounts :: MessageBatchRequestCount  -- Details about the processing state.
    , endedAt :: Maybe String           -- Optional timestamp for when processing ended.
    , createdAt :: String               -- Timestamp for when the batch was created.
    , expiresAt :: String               -- Expiration timestamp for the batch.
    , archivedAt :: Maybe String        -- Optional timestamp for when the batch was archived.
    , cancelInitiatedAt :: Maybe String -- Optional timestamp for when cancelation was initiated.
    , resultsUrl :: Maybe String        -- Optional URL for fetching detailed batch results.
    }
```

- **Purpose:** Provides metadata and processing details about the batch operation.
- **JSON Parsing:** Handles snake_case to camelCase mapping for fields like `created_at` and `results_url`.

</br>

#### `ListMessageBatchRequest`

Defines a request to list batches with optional pagination.

```haskell
data ListMessageBatchRequest = ListMessageBatchRequest
    { beforeID :: Maybe String  -- List batches created before this ID.
    , afterID :: Maybe String   -- List batches created after this ID.
    , limit :: Maybe Int        -- Maximum number of results to return.
    }
```

- **Usage:** Enables paginated retrieval of batches.
- **JSON Serialization:** Converts camelCase fields (`beforeID`, `afterID`) to snake_case for the API.

</br>

#### `ListMessageBatchResponse`

Contains the response for a request to list message batches.

```haskell
newtype ListMessageBatchResponse =
    ListMessageBatchResponse { responseData :: [MessageBatchResponse] }
```

- **Purpose:** Provides a list of batches with their metadata.
- **JSON Parsing:** Maps `data` from the API response to `responseData`.

</br>

#### `MessageBatchResult`

Represents a single result in a batch operation.

```haskell
data MessageBatchResult = MessageBatchResult
    { resultType :: String      -- Type of the result (e.g., "success" or "error").
    , message :: ChatResponse   -- The detailed response for the batch item.
    }
```

- **Usage:** Provides detailed information for each batch request.
- **JSON Parsing:** Maps the fields as needed for internal use.

</br>

#### `RetrieveMessageBatchResult`

Represents a batch result identified by its custom ID.

```haskell
data RetrieveMessageBatchResult = RetrieveMessageBatchResult
    { customID :: String            -- The custom identifier for the batch item.
    , result :: MessageBatchResult  -- The result associated with the batch item.
    }
```

- **Purpose:** Enables mapping results back to their originating requests using the custom ID.
- **JSON Parsing:** Converts snake_case fields (`custom_id`) to camelCase.

</br>

#### `RetrieveMessageBatchResults`

Encapsulates all results for a retrieved batch operation.

```haskell
newtype RetrieveMessageBatchResults =
    RetrieveMessageBatchResults [RetrieveMessageBatchResult]
```

- **Purpose:** Provides a unified structure for accessing all batch results.
- **JSON Parsing:** Supports parsing from multi-lines JSONL formats.

</br>

### ClaudeAPI.Utils

This module provides utility functions and type classes for handling query parameters and transforming strings, primarily for constructing API requests.

#### `class HasQueryParams`

Defines a type class for types that can provide query parameters for HTTP requests.

```haskell
class HasQueryParams a where 
    getBeforeID :: a -> Maybe String
    -- ^ Retrieves the `before_id` query parameter, if present.

    getAfterID  :: a -> Maybe String
    -- ^ Retrieves the `after_id` query parameter, if present.

    getLimit    :: a -> Maybe Int
    -- ^ Retrieves the `limit` query parameter, if present.
```

- **Purpose:** Used for extracting query parameters from data structures.

</br>

#### `buildQueryString`

Generates a query string from an object implementing the `HasQueryParams` type class.

```haskell
buildQueryString :: HasQueryParams a => a -> String
```

- **Input:** An object of type `a` that implements the `HasQueryParams` type class.
- **Output:** A query string (e.g., `"?before_id=123&after_id=456&limit=10"`) or an empty string if no parameters are present.
- **Usage Example:**
  ```haskell
  let query = buildQueryString myInstance
  -- Result: "?before_id=abc&limit=5"
  ```

</br>

#### `camelToUnderscore`

Converts a camelCase string to snake_case.

```haskell
camelToUnderscore :: String -> String
```

- **Input:** A camelCase string (e.g., `"camelCaseString"`).
- **Output:** A snake_case string (e.g., `"camel_case_string"`).
- **Usage Example:**
  ```haskell
  camelToUnderscore "camelCaseString"
  -- Result: "camel_case_string"
  ```

---

## Testing

Run the tests using Cabal:
```bash
cabal test
```

Test examples can be found in `test/Main.hs` and `test/ClaudeAPI`.

---

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Submit a pull request.

---

## License

This library is licensed under the MIT License. 
