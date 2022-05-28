module Slacker.Blocks.Elements.Image
  ( ImageElement(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Util (toJSONWithTypeField)

data ImageElement
  = ImageElement
  { alt_text  :: !Text
  , image_url :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance Default ImageElement where
  def
    = ImageElement
    { alt_text  = ""
    , image_url = ""
    }

instance Aeson.ToJSON ImageElement where
  toJSON
    = toJSONWithTypeField "image"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
