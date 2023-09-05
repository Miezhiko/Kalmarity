module Kalmarity.Bot.Handlers.Messages where

import           Kalmarity.Homaridae
import           Kalmarity.OpenAI
import           Kalmarity.Twitter

import           Kalmarity.Bot.Commands.Permissions
import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Constants
import           Kalmarity.Bot.Utils

import           Calamity

import           Optics

import           Control.Monad
import           Control.Monad.IO.Class             (MonadIO, liftIO)

import           Data.IORef                         (readIORef)
import qualified Data.Text                          as T
import qualified Data.Vector.Unboxing               as VU

import qualified Polysemy                           as P
import qualified Polysemy.Fail                      as P
import qualified Polysemy.Reader                    as P

--containsRussian ∷ [Char] -> Bool
--containsRussian = any (\x -> 1040 <= ord x && ord x <=1103) ∘ filter isLetter

aiResponse ∷ (Is k1 A_Getter, Is k2 A_Getter, Is k3 A_Getter,
              Is k4 A_Getter, Show a1, Show a2, JoinKinds k5 A_Getter k3,
              LabelOptic "id" k1 s s a1 a1,
              LabelOptic
                "content" k2 s s T.Text T.Text,
              LabelOptic "channelID" k4 s s a2 a2,
              LabelOptic "author" k5 s s MessageAuthor MessageAuthor,
              MonadIO m) ⇒
  T.Text → s → T.Text → m ()
aiResponse kafkaAddress kmsg inTxt =
  let msgId  = show $ kmsg ^. #id
      chanId = show $ kmsg ^. #channelID
      authId = show $ kmsg ^. #author % to (getID :: MessageAuthor → Snowflake User)
      genKey = chanId ++ "|" ++ authId ++ "|" ++ msgId
  in liftIO $ produceKafkaMessage kafkaAddress genKey inTxt

-- possibly describe fallback strategy here
openAiResponse ∷ ( BotC r
  , HasID Channel Message
  , MonadIO (P.Sem r)
  ) ⇒ Message → T.Text → T.Text → P.Sem r ()
openAiResponse kmsg inTxt modelId = do
  out <- liftIO $ openai inTxt modelId
  void $ reply kmsg out

registerMessagesHandler ∷
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.Reader Config
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) ⇒ P.Sem r () -- (Message, Maybe User, Maybe Member)
registerMessagesHandler = void $ react @'MessageCreateEvt $ \(kmsg, _mbU, mbM) ->
  let kmentions   = kmsg ^. #mentions
      tContent    = kmsg ^. #content
      kmentionIds = map (getID :: User → Snowflake User) kmentions
      authId      = kmsg ^. #author % to (getID :: MessageAuthor → Snowflake User)
  in when (authId /= ownUserId) $ do
    Just gld <- pure (kmsg ^. #guildID)
    when (ownGuildId == gld) $
      when(containsTwitterLink tContent) $
        let newText = replaceLinks tContent
            mention = T.pack $ "<@" ++ (show authId) ++ "> :\n"
            finText = mention <> newText
        in void $ reply kmsg finText
        >> invoke_ (DeleteMessage (kmsg ^. #channelID) kmsg)
    when (ownUserId `elem` kmentionIds) $ do
      let tCC = T.replace ownUserIdTxt "" tContent
      isAKafkaMode <- liftIO $ readIORef aiKafka
      kafkaAddress <- P.asks @Config $ view #kafkaAddress
      if (ownGuildId == gld)
        then 
          if isAKafkaMode
            then aiResponse kafkaAddress kmsg tCC
            else openAiResponse kmsg tCC "gpt-3.5-turbo-16k"
        else do
          isAiAllowedForAll <- liftIO $ readIORef aiForAll
          if isAiAllowedForAll
            then if isAKafkaMode 
                  then aiResponse kafkaAddress kmsg tCC
                  else openAiResponse kmsg tCC "gpt-3.5-turbo-16k"
            else do
              Just msgMem <- pure mbM
              let mRoles = msgMem ^. #roles
              if (modRoleId `VU.elem` mRoles)
                then if isAKafkaMode
                      then aiResponse kafkaAddress kmsg tCC
                      else openAiResponse kmsg tCC "gpt-3.5-turbo-16k"
                else openAiResponse kmsg tCC "llama-2-70b-chat"
