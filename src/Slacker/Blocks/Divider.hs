module Slacker.Blocks.Divider
  ( DividerBlock(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Util (toJSONWithTypeField)

data DividerBlock
  = DividerBlock
  { block_id :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Default DividerBlock where
  def
    = DividerBlock
    { block_id = Nothing
    }

instance Aeson.ToJSON DividerBlock where
  toJSON
    = toJSONWithTypeField "divider"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
