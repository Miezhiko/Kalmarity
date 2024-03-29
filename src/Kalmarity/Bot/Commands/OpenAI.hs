module Kalmarity.Bot.Commands.OpenAI
  ( registerOpenAICommand
  ) where

import           Kalmarity.OpenAI

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database

import           Calamity

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)

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
      out <- liftIO $ openai inTxt "gpt-3.5-turbo-16k"
      case out of
        Left _  -> void $ reply ctx (T.pack "we're on timeout")
        Right r -> void $ reply ctx r
