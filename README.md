#  claude-haskell 
claude-haskell is an unofficial binding for Anthropic's Claude API.

This library provides Haskell functions to interact with the Claude API, including sending text messages, images, pdf documents, listing and retrieving model information, and create message batches. It includes utilities and types for building API requests and handling responses.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Testing](#testing)
- [Contributing](#contributing)
- [License](#license)

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

### Send messages

`chat` `defaultChatRequest` `defaultIOImageChatRequest` `ChatRequest`

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

### Use a pre-defined chatbot

`chatBot`

```haskell
chatBot :: IO ()
```

**Example:**

```haskell
main :: IO ()
main = chatBot
```

### Count the number of tokens in a message

`countToken` `defaultCountTokenRequest` `CountTokenRequest`

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

### List all available models

`listModels` `defaultModelRequest`

```haskell
listModels :: ModelRequest -> IO (Either String ModelResponse)
```

```haskell
defaultModelRequest :: ModelRequest
```

### Get model details

`getModel`

```haskell
getModel :: String -> IO (Either String ModelData)
```

### Message batch operations

`createMessageBatch` `retrieveMessageBatch` `listMessageBatch` `cancelMessageBatch` `retrieveMessageBatchResults`


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

### Create custom requests to Anthropic's API

`sendRequest`

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

## Testing

Run the tests using Cabal:
```bash
cabal test
```

Test examples can be found in `test/Main.hs` and `test/ClaudeAPI`.

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Submit a pull request.

## License

This library is licensed under the MIT License. 
