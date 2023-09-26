module Kalmarity.Morning
  ( isItANiceDayForFishing
  ) where

import System.Random

isItANiceDayForFishing :: IO String
isItANiceDayForFishing = do
  let strings = [ "Morning! Nice day for fishing ain't it! Hu ha!"
                , "Morning!"
                , "Nice day for fishing"
                , "Nice day for fishing ain't it!"
                , "Hu ha!" ]
  randomIndex <- randomRIO (0, length strings - 1)
  pure (strings !! randomIndex)
