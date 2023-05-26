module Kalmarity.Bot.Commands.Permissions
  ( aiForAll
  , registerPermissionsCommand
  ) where

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Constants
import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Utils

import           Calamity

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import           Data.Default
import           Data.IORef                (IORef, atomicWriteIORef, newIORef, readIORef)

import           Optics

import qualified Polysemy                  as P
import qualified Polysemy.Reader           as P

import           System.IO.Unsafe

aiForAll ∷ IORef Bool
{-# NOINLINE aiForAll #-}
aiForAll = unsafePerformIO $ newIORef False

registerPermissionsCommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    , P.Reader Config
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) => P.Sem (DSLState FullContext r) ()
registerPermissionsCommand = do 
  void
    $ help (const "Allow AI for everyone.")
    $ commandA @'[] "allow" []
    $ \ctx -> do
      let authId = ctx ^. #user % to (getID :: User -> Snowflake User)
      when (ownerUserId == authId) $ do
        liftIO $ atomicWriteIORef aiForAll True
        tell_ @Embed ctx $ def
          & #title ?~ "Permissions"
          & #description ?~ "everyone can chat with me now"

  void
    $ help (const "Limit AI to specific role only.")
    $ commandA @'[] "forbid" []
    $ \ctx -> do
      let authId = ctx ^. #user % to (getID :: User -> Snowflake User)
      when (ownerUserId == authId) $ do
        liftIO $ atomicWriteIORef aiForAll False
        tell_ @Embed ctx $ def
          & #title ?~ "Permissions"
          & #description ?~ "Only specific people can chat with me now"

  void
    $ help (const "Check if AI mode is permissive for all.")
    $ commandA @'[] "status" []
    $ \ctx -> do
      isAiAllowedForAll <- liftIO $ readIORef aiForAll
      if isAiAllowedForAll
        then tell_ @Embed ctx $ def
              & #title ?~ "Permissions"
              & #description ?~ "AI mode is allowed for all, you can chat with me"
        else tell_ @Embed ctx $ def
              & #title ?~ "Permissions"
              & #description ?~ "Sorry but AI usage is limited for now"
