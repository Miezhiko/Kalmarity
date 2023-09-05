module Main
  ( main
  ) where

import           Kalmarity.Bot
import           Kalmarity.Homaridae
import           Kalmarity.Homaridae.BotEff

import           Calamity                               hiding (wait)
import           Calamity.Cache.Eff                     (getMessage)
import           Calamity.Cache.InMemory
import           Calamity.Commands                      hiding (path)
import           Calamity.Commands.Context
import           Calamity.Gateway
import           Calamity.Metrics.Noop
import           Calamity.Types.Model.Presence.Activity as Activity

import           Control.Concurrent
import           Control.Monad

import qualified Data.Aeson                             as Aeson
import           Data.Default
import           Data.Flags
import           Data.List
import qualified Data.Map                               as Map
import qualified Data.Text.Lazy                         as LT
import qualified Data.Yaml                              as Yaml

import qualified Database.Persist.Sql                   as DB

import qualified Df1
import qualified Di
import qualified Di.Core
import qualified DiPolysemy                             as DiP

import           Optics
import           Options.Generic

import qualified Polysemy                               as P
import qualified Polysemy.AtomicState                   as P
import qualified Polysemy.Reader                        as P
import qualified Polysemy.Time                          as P

import           System.Directory
import           System.Exit

filterLogText ∷ Df1.Level -> Df1.Message -> Bool
filterLogText Df1.Debug t = not $ "Not handling event:" `isInfixOf` (LT.unpack (Df1.unMessage t))
filterLogText _ _         = True

filterDi ∷ Di.Core.Di Di.Level Di.Path Di.Message
         -> Di.Core.Di Di.Level Di.Path Di.Message
filterDi = Di.Core.filter
  (\l p lmsg -> (Df1.Push "calamity") `notElem` p
              && filterLogText l lmsg)

messageWithSnowflake ∷ (BotC r)
                    => (Snowflake Channel, Text)
                    -> P.Sem r ()
messageWithSnowflake (chanId, txt) =
  invoke_ (CreateMessage chanId (def & #content ?~ txt))

replyWithSnowflake ∷ (BotC r, HasID Channel Message)
                  => (Snowflake Message, Text)
                  -> P.Sem r ()
replyWithSnowflake (msgId, txt) =
  getMessage msgId >>= \case
    Just msgFromId -> void $ reply @Text msgFromId txt
    Nothing        -> pure ()

-- | Run the bot with a given configuration.
runBotWith ∷ Config -> IO ()
runBotWith cfg = Di.new $ \di ->
  void
  ∘ P.runFinal
  ∘ P.embedToFinal @IO
  ∘ DiP.runDiToIO di
  ∘ DiP.local filterDi
  ∘ runCacheInMemory' 1000 -- remember 1000 messages and ids
  ∘ runMetricsNoop
  ∘ runPersistWith (cfg ^. #connectionString)
  ∘ useConstantPrefix (cfg ^. #commandPrefix)
  ∘ useFullContext
  ∘ P.runReader cfg
  ∘ P.interpretTimeGhc
  ∘ P.atomicStateToIO (MessagePointMessages Map.empty)
  ∘ runBotIO'
    (BotToken (cfg ^. #botToken))
    (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
    (Just (StatusUpdateData Nothing [botActivity] Online False))
  ∘ handleFailByLogging $ do
      when (cfg ^. #kafkaConnectOnStart) $ do
        messageIO <- bindSemToIO messageWithSnowflake
        replyIO   <- bindSemToIO replyWithSnowflake 
        void $ P.embed
            $ forkIO
            $ runKafkaConsumer (cfg ^. #kafkaAddress)
                                messageIO replyIO
      db $ DB.runMigration migrateAll
      registerBotCommands
      registerEventHandlers

-- | Run the bot in the `IO` monad, reading the configuration
-- from a `bot.json` file.
main ∷ IO ()
main = do
  opts <- unwrapRecord @_ @CLIOptions "Kalmarity bot..."
  path <- case opts ^. #config of
    Just path -> pure path
    Nothing -> do
      ifM (doesFileExist "bot.json") ("bot.json" <$ putStrLn "using bot.json...") $
        ifM (doesFileExist "bot.yaml") ("bot.yaml" <$ putStrLn "using bot.yaml...") $
          die "error: cannot find configuration file"
  cfg <-
    if "yaml" `isSuffixOf` path
    then Yaml.decodeFileThrow path
    else if "json" `isSuffixOf` path
    then Aeson.eitherDecodeFileStrict path >>= either die pure
    else die "error: unrecoognized file extension (must be either json or yaml)"
  runBotWith cfg

 where ifM mb x y = do
        b <- mb
        if b then x else y

botActivity ∷ Activity
botActivity = Activity.activity "Squids?" Game
