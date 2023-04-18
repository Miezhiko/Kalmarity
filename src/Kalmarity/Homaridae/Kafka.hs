
module Kalmarity.Homaridae.Kafka
  ( targetTopic
  ) where

import           Kafka.Producer (TopicName)

-- Topic to send messages to
targetTopic ∷ TopicName
targetTopic = "Kalmarity"
