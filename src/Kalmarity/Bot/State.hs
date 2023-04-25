module Kalmarity.Bot.State
  ( module Exported
  , serversContext
  ) where

import           Calamity

import           System.IO.Unsafe

import           Data.IORef       as Exported
import qualified Data.Map         as M
import qualified Data.Text        as T

-- servers context
serversContext ∷ IORef (M.Map (Snowflake Guild)
                              T.Text
                       )
{-# NOINLINE serversContext #-}
serversContext = unsafePerformIO $ newIORef M.empty
