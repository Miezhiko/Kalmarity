module Kalmarity.Bot.Handlers
  ( registerEventHandlers
  ) where

import           Kalmarity.Common

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Handlers.GuildCreate
import           Kalmarity.Bot.Handlers.Points
import           Kalmarity.Bot.Utils

import           Calamity
import           Calamity.Commands                     as C
import           Calamity.Commands.Context             (FullContext)

import           Control.Monad

import           Optics

import qualified Polysemy                              as P
import qualified Polysemy.AtomicState      as P
import qualified Polysemy.Fail                         as P
import qualified Polysemy.Reader                       as P
import qualified Polysemy.Time                         as P

import           TextShow

-- | Register the various event handlers for the bot.
registerEventHandlers ∷
  ( BotC r
  , P.Members
      [ Persistable
      , P.Fail
      , P.Reader Config
      , P.AtomicState MessagePointMessages
      , P.GhcTime
      ]
      r
  ) =>
  P.Sem r ()
registerEventHandlers = do
  registerCommandResponseHandler
  registerGuildCreateHandler
  registerPointGiveHandler

registerCommandResponseHandler ∷
  ( BotC r
  , P.Members
      '[ P.Fail
       , P.AtomicState MessagePointMessages
       , P.Reader Config
       ]
      r
  ) =>
  P.Sem r ()
registerCommandResponseHandler = do
  void $
    react @( 'CustomEvt (CtxCommandError FullContext)) $ \(CtxCommandError ctx e) -> do
      info $ "Command failed with reason: " <> showt e
      case e of
        ParseError n r  -> void ∘ tellt ctx $ "Failed to parse parameter: `" <> n <> "`, with reason: ```\n" <> r <> "```"
        CheckError n r  -> void ∘ tellt ctx $ "Failed to pass a check: `"    <> n <> "`, with reason: ```\n" <> r <> "```"
        InvokeError n r -> void ∘ tellt ctx $ "Failed to invoke command `"   <> n <> "`, with reason: ```\n" <> r <> "```"
      void ∘ invoke $ CreateReaction ctx ctx (UnicodeEmoji "❌")

  void $
    react @( 'CustomEvt (CommandInvoked FullContext)) $ \(CommandInvoked ctx) -> do
      info $ "Command invoked by " <> ctx ^. #user % #username
                                   <> ":\n"
                                   <> ctx ^. #message % #content
      emoj <- P.asks @Config $ view #eyesEmoji
      void ∘ invoke $ CreateReaction ctx ctx emoj
