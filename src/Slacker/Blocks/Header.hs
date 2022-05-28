module Slacker.Blocks.Header
  ( HeaderBlock(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data HeaderBlock
  = HeaderBlock
  { block_id :: !(Maybe Text)
  , text     :: !PlainTextObject
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON HeaderBlock where
  toJSON
    = toJSONWithTypeField "header"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

