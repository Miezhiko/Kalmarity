module Kalmarity.OpenAI
  ( openAIWithFallback
  , openai
  ) where

import           OpenAI.Client

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
     apiKey  <- T.pack <$> readFileStrict "/etc/chat.rs/openai.txt"
     let client = makeOpenAIClient apiKey manager 4
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

openAIWithFallback ∷ T.Text → IO T.Text
openAIWithFallback inTxt = do
  gpt35 <- openai inTxt "gpt-3.5-turbo-16k"
  case gpt35 of
    Left _ -> do
      llama <- openai inTxt "llama-2-70b-chat"
      case llama of
        Left err -> do
          print err
          pure $ T.pack "Morning! Nice day for fishing ain't it! Hu ha!"
        Right resp -> pure resp
    Right resp -> pure resp
