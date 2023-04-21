
module Kalmarity.Homaridae.Kafka
  ( consumerTopic
  , targetTopic
  ) where

import           Kafka.Producer (TopicName)

-- Topic to consume messages from
consumerTopic ∷ TopicName
consumerTopic = "Kalmarity"

-- Topic to send messages to
targetTopic ∷ TopicName
targetTopic = "Salieri"
