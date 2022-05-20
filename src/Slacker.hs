module Slacker
  ( initSocketMode
  , getNextEvent
  , handleEvents
  , runSocketMode
  , postMessage
  , postThreadReply
  , SlackConfig
  , SocketModeEnv(slackConfig)
  , ThreadError(..)
  , PostMessage(..)
  , defaultSlackConfig
  , setApiToken
  , setAppToken
  , setDebugDisconnect
  , setGracefulShutdownHandler
  , setNumThreads
  , setInboundQueueMax
  , setOnException
  , setLogLevel
  , shutdownSocketMode
  , handleThreadExceptionSensibly
  , SocketModeEvent(..)
  , pattern Event
  , EventsApiEnvelope(..)
  , HelloBody(..)
  , DisconnectBody(..)
  , Block(..)
  , imageNoTitle
  , markdownSection
  , markdown
  , plaintext
  , module Export
  ) where

import           Control.Monad.Logger.Aeson as Export

import           Slacker.Blocks
import           Slacker.Config
import           Slacker.SocketMode
import           Slacker.Web
