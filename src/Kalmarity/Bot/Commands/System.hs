module Kalmarity.Bot.Commands.System
  ( registerSystemCommand
  ) where

import           Kalmarity.Bot.Constants
import           Kalmarity.Bot.Database

import           Calamity

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import           System.Process

import           Optics

import qualified Polysemy                  as P

registerSystemCommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) => P.Sem (DSLState FullContext r) ()
registerSystemCommand = void
    $ help (const "Restart Amadeus command.")
    $ commandA @'[] "restart_amadeus" []
    $ \ctx -> do
      let authId = ctx ^. #user % to (getID :: User -> Snowflake User)
      when (ownerUserId == authId) $
        liftIO $ do
          _ <- system "sudo systemctl restart Amadeus"
          pure ()
