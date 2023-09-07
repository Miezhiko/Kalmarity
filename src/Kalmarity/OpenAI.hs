module Kalmarity.OpenAI
  ( openai
  ) where

import           OpenAI.Client

import           Servant.Client

import qualified Data.Text               as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           System.IO

request ∷ T.Text → T.Text → ChatCompletionRequest
request req modelId =
  defaultChatCompletionRequest
    (ModelId modelId)
    [ ChatMessage
        { chmRole         = "user"
        , chmContent      = Just req
        , chmFunctionCall = Nothing
        , chmName         = Nothing }
    ]

chimeraBaseUrl ∷ BaseUrl
chimeraBaseUrl = BaseUrl Https "chimeragpt.adventblocks.cc" 443 "/api"

trimNewLines ∷ String → String
trimNewLines = filter (/= '\n')

readFileStrict ∷ FilePath → IO String
readFileStrict path = do
  handle    <- openFile path ReadMode
  contents  <- hGetContents handle
  length contents `seq` hClose handle
  pure $ trimNewLines contents

openai ∷ T.Text → T.Text → IO (Either ClientError T.Text)
openai req modelId =
  do manager <- newManager tlsManagerSettings
     apiKey  <- T.pack <$> readFileStrict "/etc/chat.rs/chimera.txt"
     let client = makeOpenAIClient' chimeraBaseUrl apiKey manager 4
     result <- completeChat client (request req modelId)
     case result of
       Left failure  -> pure $ Left failure
       Right success ->
        let firstChoice = head $ chrChoices success
            finalMsg    = chchMessage firstChoice
            msgContent  = chmContent finalMsg
        in case msgContent of
            Just content -> pure $ Right content
            Nothing      -> pure $ Right (T.pack "empty response")
