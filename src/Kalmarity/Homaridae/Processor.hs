{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.Processor
  ( processKafkaMessages
  ) where

import           Calamity

import           Kalmarity.Homaridae.SnowFlake
   
import           Control.Monad

import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import           Data.Text.Encoding

import           Kafka.Consumer

processSingleMessage ∷ T.Text
                    -> T.Text
                    -> ((Snowflake Channel, T.Text) -> IO (Maybe ()))
                    -> ((Snowflake Message, T.Text) -> IO (Maybe ()))
                    -> IO ()
processSingleMessage myKey myVal msgIO replyIO = do
  case parseSnowflakesTuple myKey of
    Left err                             -> print err
    Right (channelId, userId, messageId) -> do
      case messageId of
        Snowflake 0 -> case userId of
          Snowflake 0 -> void $ msgIO (channelId, myVal)
          u -> let withMention = (T.pack $ "<@" ++ show u ++ "> ") <> myVal
               in void $ msgIO (channelId, withMention)
        m -> void $ replyIO (m, myVal)

processKafkaMessages ∷ KafkaConsumer
                    -> ((Snowflake Channel, T.Text) -> IO (Maybe ()))
                    -> ((Snowflake Message, T.Text) -> IO (Maybe ()))
                    -> IO ()
processKafkaMessages kafkaConsumer msgIO replyIO = do
  result <- pollMessage kafkaConsumer (Timeout 2000)
  case result of
    Left err ->
      case err of
        KafkaResponseError rerr ->
          case rerr of
            RdKafkaRespErrTimedOut -> pure ()
            _                      -> putStrLn $ "Polling response error: " ++ show err
        _ -> putStrLn $ "Error polling message: " ++ show err
    Right msg -> do
      let myKey = decodeUtf8 $ fromMaybe mempty (crKey msg)
          myVal = decodeUtf8 $ fromMaybe mempty (crValue msg)
      processSingleMessage myKey myVal msgIO replyIO
      _ <- commitAllOffsets OffsetCommit kafkaConsumer
      putStrLn "Offset committed"
  processKafkaMessages kafkaConsumer msgIO replyIO
