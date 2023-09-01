module Kalmarity.Bot.Commands.OpenAI
  ( registerOpenAICommand
  ) where

import           Kalmarity.OpenAI

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Utils

import           Calamity

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import           Data.Default
import qualified Data.Text                 as T

import           Optics

import qualified Polysemy                  as P
import qualified Polysemy.Reader           as P

registerOpenAICommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    , P.Reader Config
    ] r
    , MonadIO (P.Sem r)  -- Add MonadIO constraint
  ) ⇒ P.Sem (DSLState FullContext r) ()
registerOpenAICommand = void
    $ help (const "OpenAI command.")
    $ commandA @'[[T.Text]] "openai" ["o"]
    $ \ctx ltxt -> do
      Just _gld <- pure (ctx ^. #guild)
      let inTxt  = T.unwords ltxt
      out <- liftIO $ openai inTxt
      tell_ @Embed ctx $ def
          & #title ?~ "Pointfree"
          & #description ?~ out
