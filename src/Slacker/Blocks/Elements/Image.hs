{-# LANGUAGE FunctionalDependencies #-}

module Slacker.Blocks.Elements.Image
  ( ImageElement(..)
  , HasImage(..)
  , defaultImage
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Util (toJSONWithTypeField)

data ImageElement
  = ImageElement
  { alt_text  :: !Text
  , image_url :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

class HasImage arg res | res -> arg where
  image :: arg -> res
  image_ :: Text -> Text -> res

instance HasImage ImageElement ImageElement where
  image = id
  image_ url alt = defaultImage url alt

defaultImage :: Text -> Text -> ImageElement
defaultImage url alt
  = ImageElement
  { alt_text  = alt
  , image_url = url
  }

instance Aeson.ToJSON ImageElement where
  toJSON
    = toJSONWithTypeField "image"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
