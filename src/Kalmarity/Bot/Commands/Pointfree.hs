module Kalmarity.Bot.Commands.Pointfree
  ( registerPointfreeCommand
  ) where

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Utils

import           Calamity

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)

import           Control.Monad

import           Data.Default
import qualified Data.Text                 as T

import           Optics

import qualified Polysemy                  as P
import qualified Polysemy.Reader           as P

import           Pointfree

registerPointfreeCommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    , P.Reader Config
    ] r
  ) => P.Sem (DSLState FullContext r) ()
registerPointfreeCommand = void
    $ help (const "Pointfree command.")
    $ commandA @'[[T.Text]] "pointfree" ["p"]
    $ \ctx ltxt -> do
      let inTxt  = T.unwords ltxt
          pfMb   = pointfree' (T.unpack inTxt)
      Just pfRes <- pure pfMb
      let out = T.pack pfRes
      tell_ @Embed ctx $ def
          & #title ?~ "Pointfree"
          & #description ?~ out
