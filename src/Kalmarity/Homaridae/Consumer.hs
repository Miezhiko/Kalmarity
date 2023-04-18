{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.Consumer
  ( consumerProps
  , consumerSub
  , processKafkaMessages
  , runKafkaConsumer
  ) where

import           Control.Concurrent.Async
import           Control.Exception        (SomeException, bracket, catch)

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

processKafkaMessages ∷ KafkaConsumer -> IO ()
processKafkaMessages kafkaConsumer = do
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
      putStrLn $ "Received message: " ++ show msg
      _ <- commitAllOffsets OffsetCommit kafkaConsumer
      putStrLn "Offset committed"
  processKafkaMessages kafkaConsumer

-- run two workers
runConsumerSubscription ∷ KafkaConsumer -> IO (Either KafkaError ())
runConsumerSubscription kafkaConsumer = do
  workerTask1 <- async $
    catch (processKafkaMessages kafkaConsumer)
      (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)
  workerTask2 <- async $
    catch (processKafkaMessages kafkaConsumer)
      (\(e :: SomeException) -> putStrLn $ "Consumer loop exception: " ++ show e)

  (_, result) <- waitAnyCancel [workerTask1, workerTask2]
  print result

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
