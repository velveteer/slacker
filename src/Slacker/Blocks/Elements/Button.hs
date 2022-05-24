{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Elements.Button
  ( ButtonElement
  , button
  , button_
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Elements.Core
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ButtonElement
  = ButtonElement
  { bText               :: !PlainTextObject
  , bActionId           :: !Text
  , bUrl                :: !(Maybe Text)
  , bValue              :: !(Maybe Text)
  , bStyle              :: !(Maybe Text)
  , bAccessibilityLabel :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ButtonElement where
  toJSON
    = toJSONWithTypeField "button"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 1
    , Aeson.omitNothingFields = True
    }

button_ :: Text -> Text -> Elements
button_ txt actId
  = button
  ! #text txt
  ! #action_id actId
  ! defaults

button
  :: "text" :! Text
  -> "action_id" :! Text
  -> "url":? Text
  -> "value" :? Text
  -> "style" :? Text
  -> "accessibilityLabel" :? Text
  -> Elements
button (Arg txt) (Arg actId) (ArgF mUrl) (ArgF mValue) (ArgF mStyle) (ArgF mAcc)
  = element
  $ ButtonElement
  { bText               = plaintext_ txt
  , bActionId           = actId
  , bUrl                = mUrl
  , bValue              = mValue
  , bStyle              = mStyle
  , bAccessibilityLabel = mAcc
  }

