module Slacker.Blocks.Image
  ( ImageBlock(..)
  , defaultImageBlock
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ImageBlock
  = ImageBlock
  { title     :: !(Maybe PlainTextObject)
  , block_id  :: !(Maybe Text)
  , image_url :: !Text
  , alt_text  :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance HasImage ImageBlock ImageBlock where
  image = id
  image_ url alt = image $ defaultImageBlock url alt

defaultImageBlock :: Text -> Text -> ImageBlock
defaultImageBlock url alt
  = ImageBlock
  { image_url = url
  , alt_text  = alt
  , title     = Nothing
  , block_id  = Nothing
  }

instance Aeson.ToJSON ImageBlock where
  toJSON
    = toJSONWithTypeField "image"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

