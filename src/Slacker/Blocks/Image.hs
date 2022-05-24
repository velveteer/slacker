{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Image
  ( ImageBlock
  , image
  , image_
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Core
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ImageBlock
  = ImageBlock
  { ibTitle    :: !(Maybe PlainTextObject)
  , ibBlockId  :: !(Maybe Text)
  , ibImageUrl :: !Text
  , ibAltText  :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ImageBlock where
  toJSON
    = toJSONWithTypeField "image"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

-- | Create an image block.
image
  :: "image_url" :! Text
  -> "alt_text" :! Text
  -> "block_id" :? Text
  -> "title" :? Text
  -> Blocks
image (Arg url) (Arg alt) (ArgF mBlockId) (ArgF mTitle)
  = block
  $ ImageBlock
  { ibTitle    = fmap plaintext_ mTitle
  , ibBlockId  = mBlockId
  , ibImageUrl = url
  , ibAltText  = alt
  }

image_ :: Text -> Text -> Blocks
image_ url alt = image ! #image_url url ! #alt_text alt ! defaults
