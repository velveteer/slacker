{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Actions
  ( ActionsBlock
  , actions
  , actions_
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Elements
import           Slacker.Util (toJSONWithTypeField)

data ActionsBlock
  = ActionsBlock
  { abElements :: Elements
  , abBlockId  :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ActionsBlock where
  toJSON
    = toJSONWithTypeField "actions"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

actions
  :: "elements" :! Elements
  -> "block_id" :? Text
  -> ActionsBlock
actions (Arg els) (ArgF mBlockId)
  = ActionsBlock
  { abElements = els
  , abBlockId  = mBlockId
  }

actions_ :: Elements -> ActionsBlock
actions_ els = actions ! #elements els ! defaults
