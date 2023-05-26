module Kalmarity.Bot.Commands
  ( registerBotCommands
  ) where

import           Kalmarity.Bot.Commands.Help
import           Kalmarity.Bot.Commands.Homaridae
import           Kalmarity.Bot.Commands.Leaderboard
import           Kalmarity.Bot.Commands.Permissions
import           Kalmarity.Bot.Commands.Pointfree
import           Kalmarity.Bot.Commands.Utils
import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context          (FullContext)
import           CalamityCommands                   (ConstructContext, ParsePrefix)

import           Control.Monad

import qualified Polysemy                           as P
import qualified Polysemy.Fail                      as P
import qualified Polysemy.Reader                    as P
import qualified Polysemy.Time                      as P

-- | Register all the bot commands
registerBotCommands ∷
  ( BotC r
  , P.Members
    [ ParsePrefix Message
    , Persistable
    , P.Fail
    , P.GhcTime
    , P.Reader Config
    , ConstructContext (Message, User, Maybe Member) FullContext IO ()
    ] r
  ) => P.Sem r ()
registerBotCommands = void $ addCommands $ do
  admin <- isAdmin
  void customHelpCommand
  void $ requires [admin] adminHelpCommand

  registerHomaridaeCommand
  registerLeaderboardCommand
  registerPointfreeCommand
  registerPermissionsCommand
