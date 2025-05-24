module ClaudeAPI.Config where

-- | Base URL for the Anthropic API.
baseUrl :: String
baseUrl = "https://api.anthropic.com"


-- | Default model to use for requests (\"claude-3-5-sonnet-20241022\")
--
-- See [model names](https://docs.anthropic.com/en/docs/about-claude/models/overview#model-names) for other possibilities.
--
-- You can also use the 'listModels' function to get an up-to-date list of available models.
defaultModel :: String
defaultModel = "claude-3-5-sonnet-20241022"