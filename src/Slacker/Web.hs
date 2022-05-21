module Slacker.Web
  ( MessageContent(..)
  , blocks
  , blocksWithText
  , textMessage
  -- Response URL
  , MessagePayload(..)
  , respondMessage
  , ephemeralResponse
  , nonEphemeralResponse
  , ephemeralBlocks
  , nonEphemeralBlocks
  , ephemeralText
  , nonEphemeralText
  -- chat.postMessage
  , PostMessagePayload(..)
  , postMessage
  , toChannel
  , toThread
  , makeSlackPostJSON
  , makeSlackPostJSONNoBody
  ) where

import           Control.Lens hiding ((??))
import           Control.Monad (void)
import           Control.Monad.IO.Unlift (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Lens
import           Data.List.NonEmpty (NonEmpty(..))
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

data MessagePayload
  = MessagePayload
  { mpEphemeral :: !Bool
  , mpContent   :: !MessageContent
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON MessagePayload where
  toJSON MessagePayload{..} = Aeson.object $
    catMaybes
      [ if mpEphemeral
          then Just ("response_type" Aeson..= Aeson.String "ephemeral")
          else Nothing
      ] ++ messageContentFields mpContent

data PostMessagePayload
  = PostMessagePayload
  { pmpChannel  :: !Text
  , pmpThreadTs :: !(Maybe Text)
  , pmpContent  :: !MessageContent
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON PostMessagePayload where
  toJSON PostMessagePayload{..} = Aeson.object $
    catMaybes
      [ Just $ "channel" Aeson..= pmpChannel
      , fmap ("thread_ts" Aeson..=) pmpThreadTs
      ] ++ messageContentFields pmpContent

toChannel :: Text -> MessageContent -> PostMessagePayload
toChannel cid = PostMessagePayload cid Nothing

toThread :: Text -> Text -> MessageContent -> PostMessagePayload
toThread cid ts = PostMessagePayload cid (Just ts)

data MessageContent
  = MessageBlocks (Maybe Text) (NonEmpty Block)
  | MessageText Text
  deriving stock (Show, Eq, Ord)

messageContentFields :: MessageContent -> [Aeson.Pair]
messageContentFields mc =
  case mc of
    MessageBlocks mText neBlocks -> catMaybes
      [ Just $ "blocks" Aeson..= neBlocks
      , fmap ("text" Aeson..=) mText
      ]
    MessageText txt ->
      [ "text" Aeson..= txt ]

blocks :: NonEmpty Block -> MessageContent
blocks = MessageBlocks Nothing

blocksWithText :: Text -> NonEmpty Block -> MessageContent
blocksWithText txt = MessageBlocks (Just txt)

textMessage :: Text -> MessageContent
textMessage = MessageText

nonEphemeralResponse :: MessageContent -> MessagePayload
nonEphemeralResponse = MessagePayload False

nonEphemeralBlocks :: NonEmpty Block -> MessagePayload
nonEphemeralBlocks = nonEphemeralResponse . blocks

nonEphemeralText :: Text -> MessagePayload
nonEphemeralText = nonEphemeralResponse . textMessage

ephemeralResponse :: MessageContent -> MessagePayload
ephemeralResponse = MessagePayload True

ephemeralBlocks :: NonEmpty Block -> MessagePayload
ephemeralBlocks = ephemeralResponse . blocks

ephemeralText :: Text -> MessagePayload
ephemeralText = ephemeralResponse . textMessage

-- | Respond to a user using the action's response URL.
-- https://api.slack.com/interactivity/handling#message_responses
respondMessage
  :: MonadIO m
  => Text
  -- ^ Response URL from Slack.
  -> MessagePayload
  -> m ()
respondMessage url body = do
  req <- liftIO $ HTTP.parseRequest $ "POST " <> T.unpack url
  void . HTTP.httpLBS . HTTP.setRequestBodyJSON body $ req

-- | Use the `chat.postMessage` method to send a message to a particular channel.
postMessage
  :: MonadIO m
  => SlackConfig
  -> PostMessagePayload
  -> m ()
postMessage cfg payload =
  void $
    makeSlackPostJSON
      (slackApiToken cfg)
      "chat.postMessage"
      payload

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

