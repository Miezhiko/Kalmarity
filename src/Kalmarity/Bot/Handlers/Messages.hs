module Kalmarity.Bot.Handlers.Messages where

import           Kalmarity.Homaridae

import           Kalmarity.Bot.Config

import           Calamity

import           Optics

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Text              (replace, unpack)

import qualified Polysemy               as P
import qualified Polysemy.Fail          as P
import qualified Polysemy.Reader        as P

registerMessagesHandler ∷
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.Reader Config
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) => P.Sem r () -- (Message, Maybe User, Maybe Member)
registerMessagesHandler = void $ react @'MessageCreateEvt $ \(kmsg, _mbU, _mbM) ->
  let kmentions   = kmsg ^. #mentions
      kmentionIds = map (\u -> (getID :: User -> Snowflake User) u) kmentions
  -- TODO: get bot id somehow else
  in when ((Snowflake 1096396952117198868) `elem` kmentionIds) $ do
    kafkaAddress <- P.asks @Config $ view #kafkaAddress
    let msgId  = show $ kmsg ^. #id
        chanId = show $ kmsg ^. #channelID
        authId = show $ kmsg ^. #author % to (getID :: MessageAuthor -> Snowflake User)
        genKey = chanId ++ "|" ++ authId ++ "|" ++ msgId
        inTxt  = unpack $ replace "<@1096396952117198868>" "" (kmsg ^. #content)
    liftIO $ produceKafkaMessage kafkaAddress genKey inTxt
