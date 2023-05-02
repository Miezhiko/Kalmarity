{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.Processor
  ( processKafkaMessages
  ) where

import           Calamity

import           Kalmarity.Homaridae.SnowFlake
   
import           Control.Monad

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                    (fromMaybe)
import           Data.Text

import           Kafka.Consumer

processSingleMessage ∷ String
                    -> String
                    -> ((Snowflake Channel, Text) -> IO (Maybe ()))
                    -> ((Snowflake Message, Text) -> IO (Maybe ()))
                    -> IO ()
processSingleMessage myKey myVal msgIO replyIO =
  case parseSnowflakesTuple myKey of
    Left err                             -> print err
    Right (channelId, userId, messageId) ->
      case messageId of
        Snowflake 0 -> case userId of
                          Snowflake 0 -> void $ msgIO (channelId, (pack myVal))
                          u -> let withMention = "<@" ++ show u ++ "> " ++ myVal
                              in void $ msgIO (channelId, (pack withMention))
        m -> void $ replyIO (m, (pack myVal))

processKafkaMessages ∷ KafkaConsumer
                    -> ((Snowflake Channel, Text) -> IO (Maybe ()))
                    -> ((Snowflake Message, Text) -> IO (Maybe ()))
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
      let myKey = BSC.unpack $ fromMaybe BSC.empty (crKey msg)
          myVal = BSC.unpack $ fromMaybe BSC.empty (crValue msg)
      processSingleMessage myKey myVal msgIO replyIO
      _ <- commitAllOffsets OffsetCommit kafkaConsumer
      putStrLn "Offset committed"
  processKafkaMessages kafkaConsumer msgIO replyIO
