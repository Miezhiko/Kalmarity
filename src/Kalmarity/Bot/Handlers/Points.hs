module Kalmarity.Bot.Handlers.Points
  ( MessagePointMessages (..)
  , registerPointGiveHandler
  ) where

import           Kalmarity.Bot.Config
import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Schema

import           Calamity

import           Data.Map               (Map)

import           Database.Persist       as DB

import           GHC.Generics           (Generic)

import           Control.Monad

import           Optics

import qualified Polysemy               as P
import qualified Polysemy.AtomicState   as P
import qualified Polysemy.Fail          as P
import qualified Polysemy.NonDet        as P
import qualified Polysemy.Reader        as P
import qualified Polysemy.Time          as P

-- | A record of which replies have been made for which message,
-- in order to be able to delete them when necessary.
newtype MessagePointMessages
  = MessagePointMessages { messages :: Map (Snowflake Message) (Message, Int) }
  deriving (Generic, Show)

awardPoint ∷
  ( BotC r
  , P.Members
    [ Persistable
    , P.NonDet
    , P.Fail
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    ] r
  ) => Message -> User -> P.Sem r ()
awardPoint msg usr = do
  let author = msg ^. #author % to getID
      usrID  = usr ^. #id
  -- don't allow to give points to self
  guard $ author /= usrID
  Just gid <- pure (msg ^. #guildID)
  -- this is to limit giving points only to admin
  -- Right mem <- invoke $ GetGuildMember gid usr
  -- perms <- permissionsIn' gid mem
  -- guard $ perms `containsAll` administrator
  time <- P.now
  db_ $ DB.insert (MessagePoint (msg ^.  #id) gid usrID author time)

registerPointGiveHandler ∷
  ( BotC r
  , P.Members
    [ Persistable
    , P.Fail
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPointGiveHandler = void $ P.runNonDetMaybe
                                $ react @'MessageReactionAddEvt
                                $ \(msg, usr, _chan, rct) -> do
  npEmoji <- P.asks @Config $ view #pointAssignEmoji
  guard $ npEmoji == rct
  awardPoint msg usr
