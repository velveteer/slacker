{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Elements.Image
  ( ImageElement
  , imageElement
  , imageElement_
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Named

import           Slacker.Util (toJSONWithTypeField)

data ImageElement
  = ImageElement
  { iAltText  :: !Text
  , iImageUrl :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ImageElement where
  toJSON
    = toJSONWithTypeField "image"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 1
    , Aeson.omitNothingFields = True
    }

imageElement
  :: "image_url" :! Text
  -> "alt_text"  :! Text
  -> ImageElement
imageElement (Arg url) (Arg alt)
  = ImageElement
  { iImageUrl = url
  , iAltText  = alt
  }

imageElement_ :: Text -> Text -> ImageElement
imageElement_ url alt
  = imageElement
  ! #image_url url
  ! #alt_text alt
