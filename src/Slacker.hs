module Slacker
  ( initSocketMode
  , getNextEvent
  , handleEvents
  , runSocketMode
  , postMessage
  , respondMessage
  , SlackConfig
  , SocketModeEnv(slackConfig)
  , ThreadError(..)
  , MessageContent(..)
  , blocks
  , blocksJSON
  , blocks_
  , textMessage
  , MessagePayload(..)
  , response
  , ephemeral
  , PostMessagePayload(..)
  , toChannel
  , toThread
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
  , pattern Command
  , pattern Event
  , pattern Interactive
  , pattern BlockAction
  , EventsApiEnvelope(..)
  , SlashCommandsEnvelope(..)
  , SlashCommand(..)
  , HelloBody(..)
  , DisconnectBody(..)
    -- * Layout blocks
  , Blocks
  , HeaderBlock(..)
  , defaultHeader
  , header
  , header_
  , DividerBlock(..)
  , defaultDivider
  , divider
  , divider_
  , ActionsBlock(..)
  , defaultActions
  , actions
  , actions_
  , asAction
  , ContextBlock(..)
  , defaultContext
  , asContext
  , context
  , context_
  , SectionBlock(..)
  , defaultSection
  , asAccessory
  , section
  , section_
  , SectionFields
  , fields
  , ImageBlock(..)
  , defaultImageBlock
  , TextObject
  , markdown
  , plaintext
  , embolden
  , italicize
    -- * Elements
  , Elements
  , ButtonElement(..)
  , defaultButton
  , button
  , button_
  , ImageElement(..)
  , image
  , image_
  , defaultImage
  , module Export
  , IxAppend(..)
  , (!>>)
  ) where

import           Control.Monad.Logger.Aeson as Export

import           Slacker.Blocks
import           Slacker.Config
import           Slacker.SocketMode
import           Slacker.Web
