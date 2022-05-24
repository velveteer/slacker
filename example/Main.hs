{-# LANGUAGE OverloadedLabels #-}
module Main where

import           Control.Lens ((^?))
import           Control.Monad (void)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Aeson.Lens
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified System.Posix.Signals as Signals

import           Slacker

main :: IO ()
main = do
  let cfg
        = defaultSlackConfig
        & setApiToken "YOUR_API_TOKEN"
        & setAppToken "YOUR_APP_TOKEN"
        & setGracefulShutdownHandler handleShutdown
        & setOnException handleThreadExceptionSensibly
        & setLogLevel (Just LevelDebug)
  runSocketMode cfg handler

handleShutdown :: IO () -> IO ()
handleShutdown shutdown = do
  void $ Signals.installHandler Signals.sigTERM (Signals.CatchOnce shutdown) Nothing
  void $ Signals.installHandler Signals.sigINT (Signals.CatchOnce shutdown) Nothing

handler :: SlackConfig -> SocketModeEvent -> IO ()
handler cfg = \case
  Event "app_mention" evt -> void . runMaybeT $ do
    cid <- MaybeT . pure $ evt ^? key "channel" . _String
    ts  <- MaybeT . pure $ evt ^? key "ts" . _String
    msg <- MaybeT . pure $ evt ^? key "text" . _String
    postMessage cfg $ toThread cid ts mentionResponse

  Command "/magic-button" (SlashCommand { scResponseUrl = url }) -> do
    respondMessage url . response . blocks_ $
      section
        ! #text "Try clicking this magical button!"
        ! #accessory (button_ "Click me" "magic-action")
        ! defaults

  BlockAction "magic-action" val -> void . runMaybeT $ do
    url <- MaybeT . pure $ val ^? key "response_url" . _String
    respondMessage url . response $ text "You clicked the magic button!"

  _ -> pure ()

mentionResponse :: MessageContent
mentionResponse
  = blocks_
  $ header_ "Slack bot summoned"
 <> divider_
 <> section_ "You rang?"
 <> image_ "https://media.giphy.com/media/BUAxWbT6y0lJC/giphy.gif" "Hello this is dog"
