module Slacker.SocketMode.Types
  ( SocketModeEvent(..)
  , EventsApiEnvelope(..)
  , EventWrapper(..)
  , HelloBody(..)
  , DisconnectBody(..)
  , pattern Event
  ) where

import           Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_String, key)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)

data SocketModeEvent
  = EventsApi !EventsApiEnvelope
  | Interactive !Aeson.Value -- TODO
  | SlashCommands !Aeson.Value -- TODO
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

getEvent :: Aeson.Value -> Maybe (Text, Aeson.Value)
getEvent evt =
  (,) <$> evt ^? key "type" . _String
      <*> pure evt

instance Aeson.FromJSON SocketModeEvent where
  parseJSON val = flip (Aeson.withObject "SocketModeEvent") val $ \obj -> do
    typ <- obj Aeson..:? "type"
    case typ of
      Just "events_api"     -> EventsApi <$> Aeson.parseJSON val
      Just "interactive"    -> Interactive <$> Aeson.parseJSON val
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

