{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE
    ApplicativeDo
  , BlockArguments
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UndecidableInstances
  , UnicodeSyntax
  #-}

module Main
  ( main
  ) where

import           Types

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (useFullContext)
import qualified Calamity.Interactions     as I
import           Calamity.Metrics.Noop

import           Control.Concurrent
import           Control.Monad

import qualified Data.Text                 as T

import qualified Di
import qualified DiPolysemy                as DiP

import           Optics

import qualified Polysemy                  as P
import qualified Polysemy.Async            as P
import qualified Polysemy.State            as P

import           System.Environment        (getEnv)

import           TextShow

main ∷ IO ()
main = do
  tokenH <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void ∘ P.runFinal ∘ P.embedToFinal ∘ DiP.runDiToIO di
      ∘ runCacheInMemory
      ∘ runMetricsNoop
      ∘ useConstantPrefix "!"
      ∘ useFullContext
      $ runBotIO (BotToken tokenH) defaultIntents $ do
        addCommands $ do

          -- just some examples
          command @'[User] "utest" \cTX u -> do
            void ∘ tell @T.Text cTX $ "got user: " <> showt u
          command @'[Named "u" User, Named "u1" User] "utest2" \cTX u u1 -> do
            void ∘ tell @T.Text cTX $ "got user: " <> showt u <> "\nand: " <> showt u1
          command @'[T.Text, Snowflake User] "test" \_ctx something aUser -> do
            DiP.info $ "something = " <> showt something <> ", aUser = " <> showt aUser
          group "testgroup" $ do
            void $ command @'[[T.Text]] "test" \cTX l -> do
              void ∘ tell @T.Text cTX $ "you sent: " <> showt l
            group "say" do
              command @'[KleenePlusConcat T.Text] "this" \cTX msgH -> do
                void $ tell @T.Text cTX msgH
          command @'[] "explode" \_ctx -> do
            Just _ <- pure Nothing
            DiP.debug @T.Text "unreachable!"
          command @'[] "bye" \cTX -> do
            void $ tell @T.Text cTX "bye!"
            stopBot

          -- views!
          command @'[] "components" \cTX -> do
            let viewH opts = do
                  ~(add, done) <- I.row do
                    add <- I.button ButtonPrimary "add"
                    done <- I.button ButtonPrimary "done"
                    pure (add, done)
                  s <- I.select opts
                  pure (add, done, s)
            let initialState = Nephropidae 1 Nothing
            s <- P.evalState initialState $
              I.runView (viewH ["0"]) (tell cTX) \(add, done, s) -> do
                when add do
                  n <- P.gets (^. #numOptions)
                  let n' = n + 1
                  P.modify' (#numOptions .~ n')
                  let opts = map (T.pack . show) [0 .. n]
                  I.replaceView (viewH opts) (void . I.edit)

                when done do
                  finalSelected <- P.gets (^. #selected)
                  I.endView finalSelected
                  I.deleteInitialMsg
                  void ∘ I.respond $ case finalSelected of
                    Just x  -> "Thanks: " <> x
                    Nothing -> "Oopsie"

                case s of
                  Just s' -> do
                    P.modify' (#selected ?~ s')
                    void I.deferComponent
                  Nothing -> pure ()
            P.embed $ print s

          -- more views!
          command @'[] "cresponses" \cTX -> do
            let viewH = I.row do
                   a <- I.button ButtonPrimary "defer"
                   b <- I.button ButtonPrimary "deferEph"
                   c <- I.button ButtonPrimary "deferComp"
                   d <- I.button ButtonPrimary "modal"
                   pure (a, b, c, d)

                modalView = do
                  a <- I.textInput TextInputShort "a"
                  b <- I.textInput TextInputParagraph "b"
                  pure (a, b)

            I.runView viewH (tell cTX) $ \(a, b, c, d) -> do
              when a do
                void I.defer
                P.embed $ threadDelay 1000000
                void $ I.followUp @T.Text "lol"

              when b do
                void I.deferEphemeral
                P.embed $ threadDelay 1000000
                void $ I.followUpEphemeral @T.Text "lol"

              when c do
                void I.deferComponent
                P.embed $ threadDelay 1000000
                void $ I.followUp @T.Text "lol"

              when d do
                void ∘ P.async $ do
                  I.runView modalView (void . I.pushModal "lol") $ \(a', b') -> do
                    P.embed $ print (a', b')
                    void $ I.respond ("Thanks: " <> a' <> " " <> b')
                    I.endView ()

              pure ()
