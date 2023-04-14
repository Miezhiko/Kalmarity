{-# LANGUAGE
    UnicodeSyntax
  #-}

module Main where

import           System.Environment

goWithArgs ∷ [String] -> IO ()
goWithArgs _ = putStrLn "hi"

main ∷ IO ()
main = getArgs >>= goWithArgs
