module Kalmarity.Bot.Commands.Homaridae
  ( registerHomaridaeCommand
  ) where

import           Kalmarity.Homaridae

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Utils

import           Calamity

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import           Data.Default

import           Optics

import qualified Polysemy                  as P
import qualified Polysemy.Reader           as P

registerHomaridaeCommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    , P.Reader Config
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) => P.Sem (DSLState FullContext r) ()
registerHomaridaeCommand = void
    $ help (const "Homaridae command.")
    $ commandA @'[] "homaridae" []
    $ \ctx -> do
      Just _gld <- pure (ctx ^. #guild)
      kafkaAddress <- P.asks @Config $ view #kafkaAddress
      liftIO $ produceKafkaMessage kafkaAddress "produced message"
      tell_ @Embed ctx $ def
          & #title ?~ "Homaridae"
          & #description ?~ "woof"
