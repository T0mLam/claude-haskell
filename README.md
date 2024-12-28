# claude-haskell

This library provides Haskell functions to interact with Claude's API, including sending text and image messages, listing and retrieving model information. It includes utilities for building API requests and handling responses.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Modules](#modules)
  - [**ClaudeAPI.Config**](#claudeapiconfig)
    - [baseUrl](#baseurl)
  - [**ClaudeAPI.Chat**](#claudeapichat)
    - [defaultChatRequest](#defaultchatrequest)
    - [defaultCountTokenRequest](#defaultcounttokenrequest)
    - [defaultIOImageChatRequest](#defaultioimagechatrequest)
    - [sendRequest*](#sendrequest)
    - [chat*](#chat)
    - [addMessageToChatRequest](#addmessagetochatrequest)
    - [addResponseToChatRequest](#addresponsetochatrequest)
    - [chatBot*](#chatbot)
    - [countToken*](#counttoken)
    - [getMediaType](#getmediatype)
    - [encodeImageToBase64](#encodeimagetobase64)
  - [**ClaudeAPI.Models**](#claudeapimodels)
    - [defaultModelRequest](#defaultmodelrequest)
    - [listModels*](#listmodels)
    - [getModel*](#getmodel)
  - [**ClaudeAPI.Types**](#claudeapiconfig)
    - [ImageSource](#imagesource)
    - [RequestMessageContent](#requestmessagecontent)
    - [RequestMessage](#requestmessage)
    - [ChatRequest*](#chatrequest)
    - [ResponseMessage](#responsemessage)
    - [Usage](#usage)
    - [ChatResponse*](#chatresponse)
    - [ModelRequest*](#modelrequest)
    - [ModelData](#modeldata)
    - [ModelResponse*](#modelresponse)
    - [CountTokenRequest*](#counttokenrequest)
    - [CountTokenResponse*](#counttokenresponse)
- [Testing](#testing)
- [Contributing](#contributing)
- [License](#license)

---

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/your-repo/claude-haskell.git
   ```

2. Build the project:
   ```bash
   cabal build
   ```

3. Run the tests:
   ```bash
   cabal test
   ```

4. Set the API key and Anthropic version in the environment (`.env`) file:
   ```bash
   API_KEY=<api_key>
   ANTHROPIC_VERSION=<anthropic_version> # e.g. 2023-06-01
   ```

## Usage

Below is a guide to the key functionalities and corresponding functions.

**Send text messages:** 
[`chat`](#chat) [`defaultChatRequest`](#defaultchatrequest) [`ChatRequest`](#chatrequest)

**Send image messages:** 
[`chat`](#chat) [`defaultIOImageChatRequest`](#defaultioimagechatrequest) [`ChatRequest`](#chatrequest)

**Use a pre-defined chatbot:** 
[`chatBot`](#chatbot)

**Count the number of tokens in a message:**
[`countToken`](#counttoken) [`CountTokenRequest`](#counttokenrequest)

**List all available models:**
[`listModels`](#listmodels) [`defaultModelRequest`](#defaultmodelrequest)

**Get model details:**
[`getModel`](#getmodel)

**Create custom requests to Anthropic's API:**
[`sendRequest`](#sendrequest)

## Modules

### ClaudeAPI.Config

The base URL for the Claude API is defined in the `ClaudeAPI.Config` module.

#### `baseUrl`

**Type:**
```haskell
baseUrl :: String
baseUrl = "https://api.anthropic.com"
```

**Example:**
```haskell
let url = baseUrl ++ "/v1/models"
```

---

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

Sends any HTTP request to the Claude API with the specified request method, endpoint, and request body.

**Type:**
```haskell
sendRequest 
:: (ToJSON req, FromJSON resp) 
=> String 
-> String 
-> Maybe req 
-> IO (Either String resp)
```

**Parameters:**
- `requestMethod`: The HTTP method (e.g., `"POST"`, `"GET"`).
- `endpoint`: The endpoint to send the request to.
- `chatReq`: The request body, which can be a `Maybe` type.

**Returns:**
- `Right`: Contains the decoded response object if the request is successful.
- `Left`: Contains an error message if the request fails.

**Example:**
```haskell
sendRequest "POST" "/v1/messages" (Just req)
```

</br>

#### `chat`

Sends a `ChatRequest` consists of text and image to the Claude API and returns the response.

**Type:**
```haskell
chat :: ChatRequest -> IO (Either String ChatResponse)
```

**Examples:**
```haskell
chat $ defaultChatRequest "Where is the capital of China?"
```
```haskell
-- sending image with message
main :: IO (Either String ChatResponse)
main = do
    let imagePath = "https://thumbs.dreamstime.com/b/red-apple-isolated-clipping-path-19130134.jpg"
    let message = "What is in this image?"
    result <- defaultIOImageChatRequest imagePath message
    case result of
        Left err -> return $ Left err
        Right chatRequest -> chat chatRequest
```

</br>

#### `addMessageToChatRequest`

Adds a user message to the existing `ChatRequest` by appending it to the `messages` field.

**Type:**
```haskell
addMessageToChatRequest :: String -> String -> ChatRequest -> ChatRequest
```

**Parameters:**
- `r`: The role of the message sender (e.g., `"user"`).
- `m`: The message content.
- `req`: The existing `ChatRequest` object.

**Returns:**
- A new `ChatRequest` with the added message.

**Example:**
```haskell
let updatedRequest = addMessageToChatRequest "user" "Hello, Claude!" req
```

</br>

#### `addResponseToChatRequest`

Adds a response from Claude to the `ChatRequest`, updating the request's message history.

**Type:**
```haskell
addResponseToChatRequest :: ChatRequest -> ChatResponse -> ChatRequest
```

**Parameters:**
- `req`: The existing `ChatRequest` object.
- `resp`: The `ChatResponse` received from Claude.

**Returns:**
- A new `ChatRequest` with the response added.

**Example:**
```haskell
let updatedRequest = addResponseToChatRequest req chatResp
```

</br>

#### `chatBot`

Starts a simple chat with the Claude model, prompting the user for input and displaying Claude's responses. It runs in a loop until the user types `QUIT`.

**Type:**
```haskell
chatBot :: IO ()
```

**Example:**
```haskell
chatBot
```

**Output:**

```

Enter your first message (or type 'QUIT' to exit)

Claude:
-------
Hi! How can I help you today?

You:
----
...
```

</br>

#### `defaultCountTokenRequest`

Creates a default `CountTokenRequest` for counting tokens in a message, with a specified user message.

**Type:**
```haskell
defaultCountTokenRequest :: String -> CountTokenRequest
```

**Example:**
```haskell
let req = defaultCountTokenRequest "How many tokens does this message use?"
```

</br>

#### `countToken`

Sends a request to the Claude API to count the tokens used in a message.

**Type:**
```haskell
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)
```

**Example:**
```haskell
countToken req
```

</br>

#### `getMediaType`

Determines the media type (MIME type) for a file based on its extension.

**Type:**
```haskell
getMediaType :: FilePath -> String
```

**Parameters:**
- `mediaPath`: The file path of the image.

**Returns:**
- The MIME type as a `String`.

**Example:**
```haskell
let mimeType = getMediaType "image.jpg"
-- mimeType: image/jpeg
```

</br>

#### `encodeImageToBase64`

Encodes an image file (local or online) to base64 format.

**Type:**
```haskell
encodeImageToBase64 :: String -> IO (Either String Text)
```

**Parameters:**
- `imagePath`: The path to the image file (can be a URL or local path).

**Returns:**
- `Right`: Contains the base64-encoded image as `Text`.
- `Left`: Contains an error message.

**Example:**
```haskell
encodeImageToBase64 "path/to/image.jpg"
```

</br>

#### `defaultIOImageChatRequest`

Creates a `ChatRequest` with an image and text message, encoding the image in base64 format.

**Type:**
```haskell
defaultIOImageChatRequest :: String -> String -> IO (Either String ChatRequest)
```

**Parameters:**
- `imagePath`: The path to the image file.
- `message`: The text message accompanying the image.

**Returns:**
- `Right`: Contains the `ChatRequest` with the image and message.
- `Left`: Contains an error message.

**Example:**
```haskell
defaultIOImageChatRequest "path/to/image.jpg" "What is in this image?"
```

---

### ClaudeAPI.Models

This module provides functionality to list and retrieve models using the Claude API.

#### `defaultModelRequest`
Provides a default `ModelRequest` with all optional parameters set to `Nothing`.

**Type:**
```haskell
defaultModelRequest :: ModelRequest
```

**Example:**
```haskell
let request = defaultModelRequest
```

</br>

#### `listModels`
Lists available models by sending a request to the API. Optional parameters like `beforeID`, `afterID`, and `limit` can be included in the request.

**Type:**
```haskell
listModels :: ModelRequest -> IO (Either String ModelResponse)
```

**Example:**
```haskell
let request = defaultModelRequest { limit = Just 5 }
response <- listModels request
case response of
    Right models -> print models
    Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
```

</br>

#### `getModel`
Fetches detailed information about a specific model using its `modelId`.

**Type:**
```haskell
getModel :: String -> IO (Either String ModelData)
```

**Example:**
```haskell
response <- getModel "model-id-123"
case response of
    Right modelData -> print modelData
    Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
```

---

### ClaudeAPI.Types

This module defines various data types used in interacting with the Claude API, including requests, responses, and utility types.

#### `ImageSource`
Represents an image source, which includes encoding type, media type, and image data.

**Type:**
```haskell
data ImageSource = ImageSource
    { encodingType :: String
    , mediaType :: String
    , imageData :: Text
    }
```

**Example:**
```haskell
let imgSource = ImageSource { encodingType = "base64", mediaType = "image/jpeg", imageData = "<base64-encoded-data>" }
```

</br>

#### `RequestMessageContent`
Represents the content of a message, which can either be an image or text.

**Type:**
```haskell
data RequestMessageContent = 
    ImageContent { msgType :: String, source :: ImageSource } 
    | TextContent { msgType :: String, text :: String }
```

**Example:**
```haskell
let imgMsg = ImageContent { msgType = "image", source = imgSource }
let textMsg = TextContent { msgType = "text", text = "Hello" }
```

</br>

#### `RequestMessage`
Represents a message sent by a user, containing a role (e.g., "user") and content (either a string or list of `RequestMessageContent`).

**Type:**
```haskell
data RequestMessage = RequestMessage
    { role :: String
    , content :: Either String [RequestMessageContent]
    }
```

**Example:**
```haskell
let reqMsg = RequestMessage { role = "user", content = Left "Hello" }
```

</br>

#### `ChatRequest`
Represents a request to the Claude API to initiate or continue a chat. Includes model selection, messages, and various optional parameters.

**Type:**
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
```haskell
let chatReq = ChatRequest 
    { model = "claude-3-5-sonnet-20241022"
    , messages = [RequestMessage { role = "user", content = Left "Hello" }]
    , max_tokens = 1024
    , stop_sequences = Nothing
    , stream = Nothing
    , system = Nothing
    , temperature = Nothing
    }
```

</br>

#### `ResponseMessage`
Represents a response message from Claude, including the message type and text.

**Type:**
```haskell
data ResponseMessage = ResponseMessage { type_ :: String, responseText :: String }
```

**Example:**
```haskell
let respMsg = ResponseMessage { type_ = "text", responseText = "Hi, how can I help you?" }
```

</br>

#### `Usage`
Represents token usage statistics for a request, including input tokens, output tokens, and cache usage.

**Type:**
```haskell
data Usage = Usage
    { input_tokens :: Int
    , cache_creation_input_tokens :: Int
    , cache_read_input_tokens :: Int
    , output_tokens :: Int
    }
```

**Example:**
```haskell
let usage = Usage { input_tokens = 10, cache_creation_input_tokens = 5, cache_read_input_tokens = 2, output_tokens = 8 }
```

</br>

#### `ChatResponse`
Represents the response from the Claude API for a chat request, including the response ID, type, content, and usage.

**Type:**
```haskell
data ChatResponse = ChatResponse
    { id :: String
    , responseType :: String
    , responseRole :: String
    , responseContent :: [ResponseMessage]
    , responseModel :: String
    , stop_reason :: String
    , stop_sequence :: Maybe String
    , usage :: Usage
    }
```

**Example:**
```haskell
let chatResp = ChatResponse 
    { id = "response_id"
    , responseType = "text"
    , responseRole = "assistant"
    , responseContent = [ResponseMessage { type_ = "text", responseText = "Hello!" }]
    , responseModel = "claude-3-5-sonnet-20241022"
    , stop_reason = "end_of_input"
    , stop_sequence = Nothing
    , usage = usage
    }
```

</br>

#### `ModelRequest`
Represents a request for a list of models from the Claude API, with optional parameters for filtering the models.

**Type:**
```haskell
data ModelRequest = ModelRequest
    { beforeID :: Maybe String
    , afterID :: Maybe String
    , limit :: Maybe Int
    }
```

**Example:**
```haskell
let modelReq = ModelRequest { beforeID = Just "model_123", afterID = Just "model_456", limit = Just 10 }
```

</br>

#### `ModelData`
Represents a model, with its ID, type, display name, and creation date.

**Type:**
```haskell
data ModelData = Model
    { modelType :: String
    , modelID :: String
    , displayName :: String
    , createdAt :: String
    }
```

**Example:**
```haskell
let model = Model { modelType = "text", modelID = "model_123", displayName = "Claude Model", createdAt = "2024-12-01" }
```

</br>

#### `ModelResponse`
Represents the response from the Claude API for a model list request, including a list of models and pagination data.

**Type:**
```haskell
data ModelResponse = ModelResponse
    { modelData :: [ModelData]
    , hasMore :: Bool
    , firstID :: Maybe String
    , lastID :: Maybe String
    }
```

**Example:**
```haskell
let modelResp = ModelResponse { modelData = [model], hasMore = True, firstID = Just "model_123", lastID = Just "model_456" }
```

</br>

#### `CountTokenRequest`
Represents a request to count tokens used in a message.

**Type:**
```haskell
data CountTokenRequest = CountTokenRequest
    { requestMessages :: [RequestMessage]
    , model :: String
    , system :: Maybe String
    }
```

**Example:**
```haskell
let countReq = CountTokenRequest { requestMessages = [reqMsg], model = "claude-3-5-sonnet-20241022", system = Nothing }
```

</br>

#### `CountTokenResponse`
Represents the response from the Claude API for a token count request, including the number of input tokens.

**Type:**
```haskell
data CountTokenResponse = CountTokenResponse { inputTokens :: Int }
```

**Example:**
```haskell
let countResp = CountTokenResponse { inputTokens = 10 }
```

---

## Testing

Run the tests using Cabal:
```bash
cabal test
```

Test examples can be found in `test/Main.hs`.

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Submit a pull request.

## License

This library is licensed under the MIT License. 
