{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.SnowFlake
  ( parseSnowflakesTuple
  ) where

import           Calamity         hiding (parse)

import           Data.Coerce
import qualified Data.Text        as T

import           Text.Parsec
import           Text.Parsec.Text (Parser)
import           Text.Read        (readMaybe)

snowflakesTupleParser ∷ Parser ( Snowflake Channel
                               , Snowflake User
                               , Snowflake Message )
snowflakesTupleParser = do
  channel <- snowflakeParser
  _       <- char '|'
  user    <- snowflakeParser
  _       <- char '|'
  message <- snowflakeParser
  pure ( coerce channel
       , coerce user
       , coerce message )

snowflakeParser ∷ Parser (Snowflake a)
snowflakeParser = do
  snowflakeStr <- many1 digit
  case readMaybe snowflakeStr of
    Just intSnowflake -> pure $ Snowflake intSnowflake
    Nothing           -> fail "Failed to parse Snowflake"

parseSnowflakesTuple ∷ T.Text
                    -> Either ParseError ( Snowflake Channel
                                         , Snowflake User
                                         , Snowflake Message )
parseSnowflakesTuple = parse snowflakesTupleParser mempty
