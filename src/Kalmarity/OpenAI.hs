module Kalmarity.OpenAI
  ( openai
  ) where

import           OpenAI.Client

import           Servant.Client

import qualified Data.Text               as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           System.IO

request ∷ T.Text → ChatCompletionRequest
request req = ChatCompletionRequest 
         { chcrModel = ModelId "gpt-3.5-turbo-16k"
         , chcrMessages = 
            [ChatMessage { chmContent       = Just req
                         , chmRole          = "user"
                         , chmFunctionCall  = Nothing
                         , chmName          = Nothing
                         }
            ]
         , chcrTemperature = Nothing
         , chcrTopP = Nothing
         , chcrN = Nothing
         , chcrStream = Nothing
         , chcrStop = Nothing
         , chcrMaxTokens = Nothing
         , chcrPresencePenalty = Nothing
         , chcrFrequencyPenalty = Nothing
         , chcrLogitBias = Nothing
         , chcrUser = Nothing
         , chcrFunctions = Nothing
         , chcrFunctionCall = Nothing
         }

chimeraBaseUrl ∷ BaseUrl
chimeraBaseUrl = BaseUrl Https "https://chimeragpt.adventblocks.cc/api/v1" 443 ""

readFileStrict ∷ FilePath → IO String
readFileStrict path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  length contents `seq` hClose handle
  return contents

openai ∷ T.Text → IO T.Text
openai req =
  do manager <- newManager tlsManagerSettings
     apiKey  <- T.pack <$> readFileStrict "/etc/chat.rs/chimera.txt"
     let client = makeOpenAIClient' chimeraBaseUrl apiKey manager 4
     result <- completeChat client (request req)
     case result of
       Left failure  -> pure $ T.pack (show failure)
       Right success ->
        let firstChoice = head $ chrChoices success
            finalMsg    = chchMessage firstChoice
            msgContent  = chmContent finalMsg
        in case msgContent of
            Just content -> pure content
            Nothing      -> pure $ T.pack "empty response"
