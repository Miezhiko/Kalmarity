{-# LANGUAGE
    OverloadedStrings
  #-}

module Kalmarity.Homaridae.BotEff
  ( bindSemToIO
  ) where

import           Data.Functor

import qualified Polysemy       as P
import qualified Polysemy.Final as P

bindSemToIO ∷ forall r p a. P.Member (P.Final IO) r
            => (p -> P.Sem r a)
            -> P.Sem r (p -> IO (Maybe a))
bindSemToIO m = P.withStrategicToFinal $ do
  istate  <- P.getInitialStateS
  m'      <- P.bindS m
  ins     <- P.getInspectorS
  P.liftS $ pure (\x -> P.inspect ins <$> m' (istate $> x))
