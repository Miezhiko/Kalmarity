module Kalmarity.Bot.Handlers.GuildCreate where

import           Kalmarity.Common

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Utils

import           Calamity
import           Optics

import           Control.Monad

import qualified Polysemy             as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.Reader      as P

registerGuildCreateHandler ∷
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerGuildCreateHandler = void $ react @'GuildCreateEvt $ \(g, _gstatus) -> do
  mallowed <- P.asks @Config $ view #allowedGuilds
  whenJust mallowed $ \allowed ->
    when (getID g `notElem` allowed) $
      void ∘ invoke $ LeaveGuild g
