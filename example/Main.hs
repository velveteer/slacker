module Main where

import           Control.Lens              ((^?))
import           Control.Monad             (void)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Aeson.Lens
import           Data.Function             ((&))
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified System.Posix.Signals      as Signals

import           Slacker

main :: IO ()
main = do
  let cfg
        = defaultSlackConfig
        & setApiToken "YOUR_API_TOKEN"
        & setAppToken "YOUR_APP_TOKEN"
        & setGracefulShutdownHandler handleShutdown
        & setOnException handleThreadExceptionSensibly
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
    let blocks = blockResponse "I'm Mr. Meeseeks!" "Look at me!"
    postThreadReply cfg cid ts "" blocks
  Hello body -> print body
  _ -> pure ()

blockResponse :: Text -> Text -> [Block]
blockResponse title body = [titleSection, bodySection, image]
  where
    titleSection
      = markdownSection $ embolden title
    bodySection
      = markdownSection body
    image
      = imageNoTitle
        "https://media.giphy.com/media/XxvBXSD95ty37oRCl6/giphy.gif"
        "Golf advice"

embolden :: Text -> Text
embolden txt = "*" <> txt <> "*"
