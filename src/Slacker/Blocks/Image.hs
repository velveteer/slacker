module Slacker.Blocks.Image
  ( ImageBlock(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ImageBlock
  = ImageBlock
  { title     :: !(Maybe PlainTextObject)
  , block_id  :: !(Maybe Text)
  , image_url :: !Text
  , alt_text  :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ImageBlock where
  toJSON
    = toJSONWithTypeField "image"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

