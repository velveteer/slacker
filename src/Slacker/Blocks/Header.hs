{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Header
  ( HeaderBlock
  , header
  , header_
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Elements.TextObject
import           Slacker.Blocks.Core
import           Slacker.Util (toJSONWithTypeField)

data HeaderBlock
  = HeaderBlock
  { hbBlockId :: !(Maybe Text)
  , hbText    :: !PlainTextObject
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON HeaderBlock where
  toJSON
    = toJSONWithTypeField "header"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

header_ :: Text -> Blocks
header_ txt = header ! #text txt ! defaults

header
  :: "text" :! Text
  -> "block_id" :? Text
  -> Blocks
header (Arg txt) (ArgF mBlockId) = block $ HeaderBlock
  { hbBlockId = mBlockId
  , hbText    = plaintext_ txt
  }
