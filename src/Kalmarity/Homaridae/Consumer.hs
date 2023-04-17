{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.Consumer
  ( runKafkaConsumer
  ) where

import           Control.Exception (SomeException, bracket, catch)

import           Data.Text

import           Kafka.Consumer

-- Global consumer properties
consumerProps ∷ ConsumerProperties
consumerProps = brokersList ["localhost:9092"]
             <> groupId "kalmarity_group"
             <> noAutoCommit
             <> logLevel KafkaLogInfo

-- Subscription to topics
consumerSub ∷ Subscription
consumerSub = topics ["Kalmarity"]
           <> offsetReset Earliest

loopKafka ∷ KafkaConsumer -> IO ()
loopKafka kafkaConsumer = do
  result <- pollMessage kafkaConsumer (Timeout 1000)
  case result of
    Left err -> putStrLn $ "Error polling message: " ++ show err
    Right msg -> do
      putStrLn $ "Received message: " ++ show msg
      _ <- commitAllOffsets OffsetCommit kafkaConsumer
      putStrLn "Offset committed"
  loopKafka kafkaConsumer

runConsumerSubscription ∷ KafkaConsumer -> IO (Either KafkaError ())
runConsumerSubscription kafkaConsumer = do
  catch (loopKafka kafkaConsumer)
    (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)
  pure $ Right ()

runKafkaConsumer ∷ Text -> IO ()
runKafkaConsumer _kafkaAddress = do
  res <- bracket mkConsumer clConsumer runHandler
  print res
  where
    mkConsumer = newConsumer consumerProps consumerSub
    clConsumer (Left err) = return (Left err)
    clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
    runHandler (Left err) = return (Left err)
    runHandler (Right kc) = runConsumerSubscription kc
