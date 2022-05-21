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
    let bs = blockResponse "I'm Mr. Meeseeks!" "Look at me!"
    postMessage cfg $ toThread cid ts bs

  Command "/hello-meeseeks" (SlashCommand { scResponseUrl = url}) -> do
    respondMessage url . nonEphemeralBlocks $
      markdownSection
        "Try clicking this magical button!"
        <&> withAccessory (defaultButton "Click me" "button")

  BlockAction "button" val -> void . runMaybeT $ do
    url <- MaybeT . pure $ val ^? key "response_url" . _String
    respondMessage url $ nonEphemeralText "You clicked the magic button!"

  _ -> pure ()

blockResponse :: Text -> Text -> MessageContent
blockResponse title body = blocks $ titleSection <> bodySection <> image
  where
    titleSection
      = markdownSection $ embolden title
    bodySection
      = markdownSection body
    image
      = imageNoTitle
        "https://media.giphy.com/media/XxvBXSD95ty37oRCl6/giphy.gif"
        "Golf advice"
