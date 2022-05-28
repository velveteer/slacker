module Slacker.SocketMode.Types
  ( SocketModeEvent(..)
  , EventsApiEnvelope(..)
  , SlashCommandsEnvelope(..)
  , SlashCommand(..)
  , InteractiveEnvelope(..)
  , EventWrapper(..)
  , HelloBody(..)
  , DisconnectBody(..)
  , AckPayload(..)
  , pattern Command
  , pattern Event
  , pattern Interactive
  , pattern BlockAction
  ) where

import           Data.Foldable (toList)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Lens.Micro ((^?))
import           Lens.Micro.Aeson (_Array, _String, key)

data SocketModeEvent
  = EventsApi !EventsApiEnvelope
  | SlashCommands !SlashCommandsEnvelope
  | InteractiveEvent !InteractiveEnvelope
  | Hello !HelloBody
  | Disconnect !DisconnectBody
  deriving stock (Generic, Show, Eq, Ord)

-- | Helper that returns an `Aeson.Value` for a particular event type, as
-- defined in the Slack Events API: https://api.slack.com/events. There are a
-- lot of event types, and until this library finds a better way to maintain
-- static types for those events, this suffices to allow users to parse what
-- they need from any Slack event.
--
-- Author's note: I find Slack API objects hard to maintain as static types, as
-- for any large API, which is likely why we see a lot of codegen in Haskell
-- for API clients. The `slack-api` library tried to do this manually and it
-- doesn't appear to have been a viable maintenance path. Even Slack can not
-- consistently keep their API specifications updated, which convinces me that
-- the dynamic approach taken by their Bolt SDKs should also be feasible for
-- Haskell users. Maybe this library should be called `lack`.
pattern Event :: Text -> Aeson.Value -> SocketModeEvent
pattern Event typ event <-
  EventsApi (
    EventsApiEnvelope
    { eaePayload =
        EventWrapper
          { ewEvent = getEvent -> Just (typ, event)
          , ewType = "event_callback"
          }
    })

pattern Command :: Text -> SlashCommand -> SocketModeEvent
pattern Command typ cmd <-
  SlashCommands (
    SlashCommandsEnvelope
    { scePayload =
        cmd@SlashCommand
          { scCommand = typ
          }
    })

pattern Interactive :: Text -> Aeson.Value -> SocketModeEvent
pattern Interactive typ val <-
  InteractiveEvent (
    InteractiveEnvelope
      { iePayload = getEvent -> Just (typ, val)
    })

pattern BlockAction :: Text -> Aeson.Value -> SocketModeEvent
pattern BlockAction actionId val <-
  InteractiveEvent (
    InteractiveEnvelope
      { iePayload = getEvent -> Just ("block_actions", getAction -> Just (actionId, val))
    })

getEvent :: Aeson.Value -> Maybe (Text, Aeson.Value)
getEvent evt =
  (,) <$> evt ^? key "type" . _String
      <*> pure evt

getAction :: Aeson.Value -> Maybe (Text, Aeson.Value)
getAction evt = do
  [action] <- toList <$> evt ^? key "actions" . _Array
  (,) <$> (action ^? key "action_id" . _String) <*> pure evt

instance Aeson.FromJSON SocketModeEvent where
  parseJSON val = flip (Aeson.withObject "SocketModeEvent") val $ \obj -> do
    typ <- obj Aeson..:? "type"
    case typ of
      Just "events_api"     -> EventsApi <$> Aeson.parseJSON val
      Just "interactive"    -> InteractiveEvent <$> Aeson.parseJSON val
      Just "slash_commands" -> SlashCommands <$> Aeson.parseJSON val
      Just "hello"          -> Hello <$> Aeson.parseJSON val
      Just "disconnect"     -> Disconnect <$> Aeson.parseJSON val
      Just unknown          -> fail $ "Unknown socket mode event: " <> T.unpack unknown
      _                     -> fail "No socket mode type"

data EventsApiEnvelope
  = EventsApiEnvelope
  { eaeAcceptsResponsePayload :: !Bool
  , eaeEnvelopeId             :: !Text
  , eaeRetryAttempt           :: !Int
  , eaeRetryReason            :: !Text
  , eaePayload                :: !EventWrapper
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON EventsApiEnvelope where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
    }

data SlashCommandsEnvelope
  = SlashCommandsEnvelope
  { sceAcceptsResponsePayload :: !Bool
  , sceEnvelopeId             :: !Text
  , scePayload                :: !SlashCommand
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON SlashCommandsEnvelope where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
    }

data SlashCommand
  = SlashCommand
  { scTeamId              :: !Text
  , scTeamDomain          :: !Text
  , scChannelId           :: !Text
  , scChannelName         :: !Text
  , scUserId              :: !Text
  , scCommand             :: !Text
  , scText                :: !Text
  , scApiAppId            :: !Text
  , scIsEnterpriseInstall :: !Text
  , scResponseUrl         :: !Text
  , scTriggerId           :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON SlashCommand where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data InteractiveEnvelope
  = InteractiveEnvelope
  { ieAcceptsResponsePayload :: !Bool
  , ieEnvelopeId             :: !Text
  , iePayload                :: !Aeson.Value
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON InteractiveEnvelope where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data EventWrapper
  = EventWrapper
  { ewToken          :: !Text
  , ewTeamId         :: !Text
  , ewApiAppId       :: !Text
  , ewEvent          :: !Aeson.Value
  -- ^ TODO Lots of event types to model.
  , ewType           :: !Text
  , ewEventId        :: !Text
  , ewEventTime      :: !Int
  , ewAuthorizations :: ![Authorization]
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON EventWrapper where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data Authorization
  = Authorization
  { aEnterpriseId :: !(Maybe Text)
  , aTeamId       :: !(Maybe Text)
  , aUserId       :: !(Maybe Text)
  , aIsBot        :: !Bool
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON Authorization where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 1
    }

data HelloBody
  = HelloBody
  { connectionInfo :: !ConnectionInfo
  , debugInfo      :: !DebugInfo
  , numConnections :: !Int
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON HelloBody where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

data ConnectionInfo
  = ConnectionInfo
  { appId :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON ConnectionInfo where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

data DisconnectBody
  = DisconnectBody
  { deType      :: !Text
  , deReason    :: !Text
  , deDebugInfo :: !DebugInfo
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON DisconnectBody where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data DebugInfo
  = DebugInfo
  { diHost                      :: !(Maybe Text)
  , diStarted                   :: !(Maybe Text)
  , diBuildNumber               :: !(Maybe Int)
  , diApproximateConnectionTime :: !(Maybe Int)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.FromJSON DebugInfo where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data AckPayload =
  AckPayload
    { envelopeId :: !Text
    , payload    :: !(Maybe Aeson.Value)
    }
  deriving stock (Generic, Eq, Show)

instance Aeson.ToJSON AckPayload where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    , Aeson.omitNothingFields = True
    }
