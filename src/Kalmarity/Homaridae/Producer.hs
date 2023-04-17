module Kalmarity.Homaridae.Producer
  ( produceKafkaMessage
  ) where

import           Control.Exception     (bracket)
import           Control.Monad         (forM_)

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Text             (Text)

import           Kafka.Producer

-- Global producer properties
producerProps ∷ ProducerProperties
producerProps = brokersList ["localhost:9092"]
             <> sendTimeout (Timeout 10000)
             <> setCallback (deliveryCallback print)
             <> logLevel KafkaLogDebug

-- Topic to send messages to
targetTopic ∷ TopicName
targetTopic = "Kalmarity"

mkMessage ∷ Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v = ProducerRecord
                  { prTopic     = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey       = k
                  , prValue     = v
                  , prHeaders   = mempty
                  }

sendMessage ∷ String -> KafkaProducer -> IO (Either KafkaError ())
sendMessage msg prod = do
  err1 <- produceMessage prod (mkMessage (Just "MSG") (Just $ pack msg))
  forM_ err1 print
  pure $ Right ()

produceKafkaMessage ∷ String -> IO ()
produceKafkaMessage msg =
    bracket mkProducer clProducer runHandler >>= print
    where
      mkProducer = newProducer producerProps
      clProducer (Left _)     = return ()
      clProducer (Right prod) = closeProducer prod
      runHandler (Left err)   = return $ Left err
      runHandler (Right prod) = sendMessage msg prod
