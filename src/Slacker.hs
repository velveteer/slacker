module Slacker
  ( -- * Socket Mode
    initSocketMode
  , runSocketMode
  , shutdownSocketMode
  , getNextEvent
  , handleEvents
    -- * Configuration
  , SlackConfig
  , SocketModeEnv(slackConfig)
  , defaultSlackConfig
  , setApiToken
  , setAppToken
  , setDebugDisconnect
  , setGracefulShutdownHandler
  , setNumThreads
  , setInboundQueueMax
  , setOnException
  , setLogLevel
  , handleThreadExceptionSensibly
  , ThreadError(..)
  , SocketModeEvent(..)
  , pattern Command
  , pattern Event
  , pattern EventResult
  , pattern EventValue
  , pattern Interactive
  , pattern BlockAction
  , EventsApiEnvelope(..)
  , SlashCommandsEnvelope(..)
  , SlashCommand(..)
  , HelloBody(..)
  , DisconnectBody(..)
    -- * Events API Payloads
  , AppMention(..)
    -- * Web API
  , postMessage
  , respondMessage
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
  -- ** File Uploading
  , File(..)
  , FilesUpload(..)
  , FileContent(..)
  , FileType(..)
  , defaultFilesUpload
  , content
  , filepath
  , filesUpload
  , uploadFile
  , uploadJSON
  , uploadJSONText
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
  , SectionFields(..)
  , field
  , HasFields(..)
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
import           Slacker.Events
import           Slacker.SocketMode
import           Slacker.Web
