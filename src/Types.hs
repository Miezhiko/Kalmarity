{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  , UnicodeSyntax
  #-}

module Types
  ( module Exported
  , Nephropidae (..)
  ) where

import           Optics.TH       (makeFieldLabelsNoPrefix)

import           Prelude.Unicode as Exported

import qualified Data.Text       as T

data Nephropidae
  = Nephropidae
      { numOptions :: Int
      , selected   :: Maybe T.Text
      }

$(makeFieldLabelsNoPrefix ''Nephropidae)
