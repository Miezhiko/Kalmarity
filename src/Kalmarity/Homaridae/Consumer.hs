{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.Consumer
  ( consumerProps
  , consumerSub
  , processKafkaMessages
  , runKafkaConsumer
  ) where

import           Calamity

import           Kalmarity.Homaridae.Kafka
import           Kalmarity.Homaridae.SnowFlake

import           Control.Concurrent.Async
import           Control.Exception             (SomeException, bracket, catch)
   
import           Control.Monad

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                    (fromMaybe)
import           Data.Text

import           Kafka.Consumer

-- Global consumer properties
consumerProps ∷ Text -> ConsumerProperties
consumerProps addr = brokersList [BrokerAddress addr]
                  <> groupId "kalmarity_group"
                  <> noAutoCommit
                  <> logLevel KafkaLogInfo

-- Subscription to topics
consumerSub ∷ Subscription
consumerSub = topics [consumerTopic]
           <> offsetReset Earliest

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
      case parseSnowflakesTuple myKey of
        Left err                         -> print err
        Right (_channelId, _user, messageId) -> do
          void $ replyIO (messageId, (pack myVal))
      _ <- commitAllOffsets OffsetCommit kafkaConsumer
      putStrLn "Offset committed"
  processKafkaMessages kafkaConsumer msgIO replyIO

-- run two workers
runConsumerSubscription ∷ KafkaConsumer
                       -> ((Snowflake Channel, Text) -> IO (Maybe ()))
                       -> ((Snowflake Message, Text) -> IO (Maybe ()))
                       -> IO (Either KafkaError ())
runConsumerSubscription kafkaConsumer msgIO replyIO = do
  workerTask1 <- async $
    catch (processKafkaMessages kafkaConsumer msgIO replyIO)
      (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)
  workerTask2 <- async $
    catch (processKafkaMessages kafkaConsumer msgIO replyIO)
      (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)

  (_, result) <- waitAnyCancel [workerTask1, workerTask2]
  print result

  pure $ Right ()

runKafkaConsumer ∷ Text
                -> ((Snowflake Channel, Text) -> IO (Maybe ()))
                -> ((Snowflake Message, Text) -> IO (Maybe ()))
                -> IO ()
runKafkaConsumer kafkaAddress msgIO replyIO = do
  res <- bracket mkConsumer clConsumer runHandler
  print res
  where
    mkConsumer = newConsumer (consumerProps kafkaAddress) consumerSub
    clConsumer (Left err) = return (Left err)
    clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
    runHandler (Left err) = return (Left err)
    runHandler (Right kc) = runConsumerSubscription kc msgIO replyIO
