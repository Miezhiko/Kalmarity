module Kalmarity.Bot.Commands.Leaderboard
  ( registerLeaderboardCommand
  ) where

import           Kalmarity.Bot.Database
import           Kalmarity.Bot.Schema
import           Kalmarity.Bot.Utils

import           Calamity

import           Database.Persist          as DB

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           Control.Arrow             ((&&&))
import           Control.Monad

import           Data.Default
import           Data.List
import qualified Data.Map                  as Map
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Traversable

import           Optics

import qualified Polysemy                  as P

import           TextShow

-- | Format a point value to a phrase with proper grammer.
showPoints ∷ Int -> Text
showPoints 0 = "no lobsters"
showPoints 1 = "1 lobster"
showPoints n = showt n <> " lobsters"

registerLeaderboardCommand ∷
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  ) => P.Sem (DSLState FullContext r) ()
registerLeaderboardCommand = void
    $ help (const "Shows the top lobsters leadearboard.")
    $ commandA @'[] "leaderboard" ["top", "lobsters"]
    $ \ctx -> do
    Just gld <- pure (ctx ^. #guild)
    messagePointsRaw <- db $ selectList [MessagePointGuild ==. getID gld] [Asc MessagePointAssignedTo]
    freePointsRaw <- db $ selectList [FreePointGuild ==. getID gld] [Asc FreePointAssignedTo]
    let messagePointsMap
          = Map.fromListWith (+)
          ∘ fmap (,1)
          $ messagePointAssignedTo ∘ entityVal <$> messagePointsRaw
        freePointsMap
          = Map.fromListWith (+)
          $ (freePointAssignedTo &&& freePointAmount) ∘ entityVal <$> freePointsRaw
        pointsMap = Map.unionWith (+) messagePointsMap freePointsMap
        topFive = take 5 ∘ sortOn (Down ∘ snd) ∘ Map.toList $ pointsMap
    points <- for topFive $ \(u, p) -> do
      Just usr <- upgrade u
      pure (usr ^. #username, p)

    let txt = T.unlines [showt i <> ". "
                                 <> nm
                                 <> ": "
                                 <> showPoints p | (i, (nm, p)) <- zip [(1 :: Int)..] points]
    if null points
      then tellt_ ctx "No one has any points yet!"
      else tell_ @Embed ctx $ def
        & #title ?~ "Leaderboard"
        & #description ?~ txt
