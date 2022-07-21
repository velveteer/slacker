{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (void)
import           Data.Function ((&))
import           Lens.Micro ((^?!))
import           Lens.Micro.Aeson (_String, key)
import qualified System.Posix.Signals as Signals

import           Slacker
import qualified Slacker as S

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
  Event AppMention{..} ->
    postMessage cfg . toThread channel ts . blocks_ $ S.do
      header_ "Slack bot summoned!"
      divider_
      image_ "https://media.giphy.com/media/BUAxWbT6y0lJC/giphy.gif" "Hello this is dog"

  Command "/magic-button" (SlashCommand { scResponseUrl = url }) ->
    respondMessage url . response . blocks_ .
      section_ $ S.do
        "Try clicking this magical button!"
        button_ "Click me" "magic-action"

  BlockAction "magic-action" val -> do
    let url = val ^?! key "response_url" . _String
    respondMessage url "You clicked the magic button!"

  evt -> print evt
