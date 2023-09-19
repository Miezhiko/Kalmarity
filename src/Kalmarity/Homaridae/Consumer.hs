{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.Consumer
  ( consumerProps
  , consumerSub
  , runKafkaConsumer
  ) where

import           Kalmarity.Homaridae.Kafka
import           Kalmarity.Homaridae.Processor

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception             (SomeException, bracket, catch)

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

-- run two workers
runConsumerSubscription ∷ KafkaConsumer
                       -> MsgIO
                       -> RepIO
                       -> IO (Either KafkaError ())
runConsumerSubscription kafkaConsumer msgIO replyIO = do
  workerTask1 <- async $
    catch (processKafkaMessages kafkaConsumer msgIO replyIO)
      (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)

  -- run second worker second afer
  -- each worker poll rate is 500ms (was 2s)
  threadDelay 1000000

  workerTask2 <- async $
    catch (processKafkaMessages kafkaConsumer msgIO replyIO)
      (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)

  (_, result) <- waitAnyCancel [workerTask1, workerTask2]
  print result

  pure $ Right ()

runKafkaConsumer ∷ Text
                -> MsgIO
                -> RepIO
                -> IO ()
runKafkaConsumer kafkaAddress msgIO replyIO = do
  res <- bracket mkConsumer clConsumer runHandler
  print res
  where
    mkConsumer = newConsumer (consumerProps kafkaAddress) consumerSub
    clConsumer (Left err) = pure (Left err)
    clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
    runHandler (Left err) = pure (Left err)
    runHandler (Right kc) = runConsumerSubscription kc msgIO replyIO
