module Kalmarity.Homaridae.Producer
  ( produceKafkaMessage
  ) where

import           Kalmarity.Homaridae.Kafka

import           Control.Exception         (bracket)
import           Control.Monad             (forM_)

import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (pack)
import           Data.Text                 (Text)
import           Data.Text.Encoding

import           Kafka.Producer

-- Global producer properties
producerProps ∷ Text -> ProducerProperties
producerProps addr = brokersList [BrokerAddress addr]
                  <> sendTimeout (Timeout 10000)
                  <> setCallback (deliveryCallback print)
                  <> logLevel KafkaLogDebug

mkMessage ∷ Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v = ProducerRecord
                  { prTopic     = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey       = k
                  , prValue     = v
                  , prHeaders   = mempty
                  }

sendMessage ∷ String -> Text -> KafkaProducer -> IO (Either KafkaError ())
sendMessage key msg prod = do
  err1 <- produceMessage prod (mkMessage (Just $ pack key)
                                         (Just $ encodeUtf8 msg))
  forM_ err1 print
  pure $ Right ()

produceKafkaMessage ∷ Text -> String -> Text -> IO ()
produceKafkaMessage kafkaAddress key msg =
    bracket mkProducer clProducer runHandler >>= print
    where
      mkProducer = newProducer $ producerProps kafkaAddress
      clProducer (Left _)     = pure ()
      clProducer (Right prod) = closeProducer prod
      runHandler (Left err)   = pure $ Left err
      runHandler (Right prod) = sendMessage key msg prod
