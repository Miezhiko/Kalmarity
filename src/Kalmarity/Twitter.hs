{-# LANGUAGE
    Safe
  #-}

module Kalmarity.Twitter
  ( containsTwitterLink
  , replaceLinks
  ) where

import           Kalmarity.Common

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec
import           Text.Parsec.Text (Parser)

twitterLinkParser ∷ Parser Text
twitterLinkParser = do
  protocol <- try (string "https://")     <|> string "http://"
  domain   <- try (string "twitter.com/") <|> string "x.com/"
  username <- many1 (alphaNum <|> char '_')
  _        <- char '/'
  _        <- string "status/"
  tweetID  <- many1 digit
  let urlPrefix = protocol ++ domain
  pure $ T.pack $ urlPrefix ++ username ++ "/status/" ++ tweetID

containsTwitterLink ∷ Text -> Bool
containsTwitterLink msg =
  case parse (many (choice [try twitterLinkParser, anyChar >> pure T.empty])) "" msg of
    Left _      -> False
    Right links -> any (not . T.null) links

replaceLinks ∷ Text -> Text
replaceLinks = T.replace "x.com" "vxtwitter.com"
             ∘ T.replace "twitter.com" "vxtwitter.com"
