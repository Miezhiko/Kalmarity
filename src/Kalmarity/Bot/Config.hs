{-# LANGUAGE
    DeriveAnyClass
  , DerivingStrategies
  , DuplicateRecordFields
  , MultiParamTypeClasses
  , TemplateHaskell
  , UndecidableInstances
  #-}

module Kalmarity.Bot.Config
  ( CLIOptions (..)
  , Config (..)
  ) where

import           Calamity

import           Data.Aeson

import           GHC.Generics

import           Optics
import           Options.Generic

-- | CLI options
newtype CLIOptions w
  = Options { config :: w ::: Maybe FilePath <?> "The path to the configuration file (must end with .json/.yaml)" }
  deriving (Generic)

instance ParseRecord (CLIOptions Wrapped)

-- | The application configuration
data Config
  = Config
      { botToken            :: Text
      , commandPrefix       :: Text
      , connectionString    :: Text
      , kafkaAddress        :: Text
      , kafkaConnectOnStart :: Bool
      , eyesEmoji           :: RawEmoji
      , pointAssignEmoji    :: RawEmoji
      , allowedGuilds       :: Maybe [Snowflake Guild]
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

makeFieldLabelsNoPrefix ''Config
