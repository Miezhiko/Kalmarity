module Kalmarity.Bot.Handlers.GuildCreate where

import           Kalmarity.Common

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.State
import           Kalmarity.Bot.Utils

import           Calamity

import           Optics

import           Control.Monad
import           Control.Monad.IO.Class           (MonadIO, liftIO)

import qualified Data.Map                         as M

import qualified Polysemy                         as P
import qualified Polysemy.Fail                    as P
import qualified Polysemy.Reader                  as P

registerGuildCreateHandler ∷
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.Reader Config
    ] r
  , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) => P.Sem r ()
registerGuildCreateHandler = void $ react @'GuildCreateEvt $ \(g, _gstatus) -> do
  mallowed <- P.asks @Config $ view #allowedGuilds
  whenJust mallowed $ \allowed ->
    let guildId = getID g
    in if (guildId `notElem` allowed)
      then void ∘ invoke $ LeaveGuild g
      else liftIO $ do
        myServersContext <- readIORef serversContext
        unless (M.member guildId myServersContext) $
          let chans = g ^. #name
              new   = M.insert guildId chans myServersContext
          in writeIORef serversContext new
