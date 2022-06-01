module Slacker.Blocks.Divider
  ( DividerBlock(..)
  , defaultDivider
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Util (toJSONWithTypeField)

data DividerBlock
  = DividerBlock
  { block_id :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

defaultDivider :: DividerBlock
defaultDivider = DividerBlock Nothing

instance Aeson.ToJSON DividerBlock where
  toJSON
    = toJSONWithTypeField "divider"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
