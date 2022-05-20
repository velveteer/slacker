module Slacker.Web
  ( PostMessage(..)
  , postMessage
  , postThreadReply
  , makeSlackPostJSON
  , makeSlackPostJSONNoBody
  ) where

import           Control.Lens hiding ((??))
import           Control.Monad (void)
import           Control.Monad.IO.Unlift (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import           UnliftIO.Exception

import           Slacker.Blocks (Block(..))
import           Slacker.Config (SlackConfig(..))

data PostMessage
  = PostMessage
  { pmChannel  :: !Text
  , pmThreadTs :: !(Maybe Text)
  , pmContent  :: !MessageContent
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON PostMessage where
  toJSON PostMessage{..} = Aeson.object $
    catMaybes
      [ Just $ "channel" Aeson..= pmChannel
        , fmap ("thread_ts" Aeson..=) pmThreadTs
      ] ++ contentFields
    where
      contentFields
        = case pmContent of
            MessageBlocks blocks mText -> catMaybes
              [ Just $ "blocks" Aeson..= blocks
              , fmap ("text" Aeson..=) mText
              ]
            MessageText txt ->
              [ "text" Aeson..= txt ]

instance Aeson.FromJSON PostMessage where
  parseJSON = Aeson.withObject "PostMessage" $ \obj -> do
    mBlocks <- obj Aeson..:? "blocks"
    PostMessage
      <$> obj Aeson..: "channel"
      <*> obj Aeson..: "thread_ts"
      <*> (case mBlocks of
            Nothing     -> MessageText <$> obj Aeson..: "text"
            Just blocks -> MessageBlocks blocks <$> obj Aeson..:? "text")

data MessageContent
  = MessageBlocks (NonEmpty Block) (Maybe Text)
  | MessageText Text
  deriving stock (Show, Eq, Ord)

mkMessageContent :: Text -> [Block] -> MessageContent
mkMessageContent txt blocks =
  case nonEmpty blocks of
    Just blocks' -> MessageBlocks blocks' (Just txt)
    Nothing      -> MessageText txt

postThreadReply
  :: MonadIO m
  => SlackConfig
  -> Text
  -- ^ Channel id
  -> Text
  -- ^ ts of another message that we're replying to
  -> Text
  -- ^ Message text, usually a fallback if you are providing blocks.
  -> [Block]
  -- ^ Slack blocks, you can use them to build nicer responses.
  -> m ()
postThreadReply cfg cid ts = postMessageImpl cfg cid (Just ts)

postMessage
  :: MonadIO m
  => SlackConfig
  -> Text
  -- ^ Channel id
  -> Text
  -- ^ Message text, usually a fallback if you are providing blocks.
  -> [Block]
  -- ^ Slack blocks, you can use them to build nicer responses.
  -> m ()
postMessage cfg cid = postMessageImpl cfg cid Nothing

postMessageImpl
  :: MonadIO m
  => SlackConfig
  -> Text
  -- ^ Channel id
  -> Maybe Text
  -- ^ optional ts of another message that we're replying to
  -> Text
  -- ^ Message text, usually a fallback if you are providing blocks.
  -> [Block]
  -- ^ Slack blocks, you can use them to build nicer responses.
  -> m ()
postMessageImpl cfg cid ts txt blocks =
  void $
    makeSlackPostJSON
      (slackApiToken cfg)
      "chat.postMessage"
      payload
  where
    payload
      = PostMessage
      { pmChannel  = cid
      , pmThreadTs = ts
      , pmContent  = mkMessageContent txt blocks
      }

makeSlackPostJSON
  :: (MonadIO m, Aeson.ToJSON val)
  => Text
  -> Text
  -> val
  -> m Aeson.Value
makeSlackPostJSON token method body =
  makeSlackPostJSONImpl token method (Just body)

makeSlackPostJSONNoBody
  :: (MonadIO m)
  => Text
  -> Text
  -> m Aeson.Value
makeSlackPostJSONNoBody token method =
  makeSlackPostJSONImpl token method (Nothing @(Maybe ()))

makeSlackPostJSONImpl
  :: (MonadIO m, Aeson.ToJSON val)
  => Text
  -> Text
  -> Maybe val
  -> m Aeson.Value
makeSlackPostJSONImpl auth method mBody = do
  req <- liftIO $ HTTP.parseRequest $ "POST " <> "https://slack.com/api/" <> T.unpack method
  resp
    <- fmap HTTP.responseBody
     . HTTP.httpJSON
     . HTTP.addRequestHeader "Authorization" ("Bearer " <> T.encodeUtf8 auth)
     . maybe id HTTP.setRequestBodyJSON mBody
     $ req
  case resp ^? key "ok"  . _Bool of
    Just True  -> pure resp
    Just False -> throwIO $ userError $ T.unpack $ resp ^. key "error" . _String
    Nothing    -> throwIO $ userError "Couldn't parse key 'ok' from response"

