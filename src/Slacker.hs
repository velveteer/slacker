module Slacker
  ( -- * Socket Mode
    runSocketMode
  , handleEvents
  , initSocketMode
  , shutdownSocketMode
  , getNextEvent
    -- ** Configuration
  , SlackConfig(..)
  , SocketModeEnv(..)
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
    -- * Events API
  , AppMention(..)
  , Edited(..)
    -- * Web API
  , ApiToken
  , Method
  , makeSlackPostJSON
  , makeSlackPostJSONNoBody
    -- ** Messaging
  , postMessage
  , respondMessage
  , MessageContent(..)
  , blocks
  , blocksJSON
  , blocks_
  , textMessage
  , ResponsePayload(..)
  , response
  , ephemeral
  , textResponse
  , PostMessagePayload(..)
  , toChannel
  , toThread
  , Channel
  , Emoji
  , Timestamp
  , addReaction
    -- ** File Uploading
  , File(..)
  , FilesUpload(..)
  , FileContent(..)
  , FileType(..)
  , Filename
  , filesUpload
  , defaultFilesUpload
  , content
  , filepath
  , uploadContent
  , uploadFile
  , uploadJSON
  , uploadJSONText
    -- * Block Kit
    -- ** Layout blocks
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
    -- ** Composition objects
  , TextObject
  , markdown
  , plaintext
  , embolden
  , italicize
    -- ** Elements
  , Elements
  , ButtonElement(..)
  , defaultButton
  , button
  , button_
  , ImageElement(..)
  , image
  , image_
  , defaultImage
  -- ** Re-exports
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
