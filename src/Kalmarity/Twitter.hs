{-# LANGUAGE
    Safe
  #-}

module Kalmarity.Twitter
  ( containsTwitterLink
  , replaceLinks
  ) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec
import           Text.Parsec.Text (Parser)

twitterLinkParser ∷ Parser Text
twitterLinkParser = do
  _ <- string "https://twitter.com/"
  username <- many1 (alphaNum <|> char '_')
  _ <- char '/'
  _ <- string "status/"
  tweetID <- many1 digit
  return $ T.pack $ "https://twitter.com/" ++ username ++ "/status/" ++ tweetID

containsTwitterLink ∷ Text -> Bool
containsTwitterLink msg =
  case parse (many (choice [try twitterLinkParser, anyChar >> return T.empty])) "" msg of
    Left _      -> False
    Right links -> any (not . T.null) links

replaceLinks ∷ Text -> Text
replaceLinks = T.replace "twitter.com" "vxtwitter.com"
