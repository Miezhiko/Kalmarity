module Kalmarity.Bot.Commands.Homaridae
  ( registerHomaridaeCommand
  ) where

import           Kalmarity.Homaridae

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

registerHomaridaeCommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    --, Embed IO ?
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) => P.Sem (DSLState FullContext r) ()
registerHomaridaeCommand = void
    $ help (const "Homaridae command.")
    $ commandA @'[] "homaridae" ["lb"]
    $ \ctx -> do
      Just _gld <- pure (ctx ^. #guild)
      liftIO $ produceKafkaMessage "produced message"
      tell_ @Embed ctx $ def
          & #title ?~ "Homaridae"
          & #description ?~ "woof"
