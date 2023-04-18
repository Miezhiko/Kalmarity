{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , QuasiQuotes
  , RecordWildCards
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}

module Kalmarity.Bot.Schema where

import           Kalmarity.Bot.Orphans ()

import           Calamity

import           Data.Time

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

newtype PersistFieldEnum a
  = PersistFieldEnum a
  deriving newtype (Bounded, Enum, Eq, Ord)

instance (Enum (PersistFieldEnum a)
   , Bounded (PersistFieldEnum a)
   , Ord (PersistFieldEnum a)) => PersistField (PersistFieldEnum a) where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 n) = Right $ toEnum (fromIntegral n)
  fromPersistValue _                = Left "Error parsing enum field" -- TODO better message

instance (Enum (PersistFieldEnum a), Bounded (PersistFieldEnum a), Ord (PersistFieldEnum a)) => PersistFieldSql (PersistFieldEnum a) where
  sqlType _ = SqlInt64

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  MessagePoint
    message (Snowflake Message)
    guild (Snowflake Guild)
    assignedBy (Snowflake User)
    assignedTo (Snowflake User)
    assignedAt UTCTime
    deriving Show

  FreePoint
    guild (Snowflake Guild)
    assignedBy (Snowflake User)
    assignedTo (Snowflake User)
    assignedAt UTCTime
    amount Int
    deriving Show
|]
