module Kalmarity.Bot.Constants where

import           Calamity

import qualified Data.Text as T

ownUserId ∷ Snowflake User
ownUserId = Snowflake 1096396952117198868

ownUserIdTxt ∷ T.Text
ownUserIdTxt = "<@1096396952117198868>"

ownerUserId ∷ Snowflake User
ownerUserId = Snowflake 510368731378089984

ownGuildId ∷ Snowflake Guild
ownGuildId = Snowflake 611822838831251466

modRoleId ∷ Snowflake Role
modRoleId = Snowflake 907158486016729088
