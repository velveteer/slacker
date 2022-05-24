module Slacker.Blocks.Divider
  ( DividerBlock
  , divider
  , divider_
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Core
import           Slacker.Util (toJSONWithTypeField)

data DividerBlock
  = DividerBlock
  { dbBlockId :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON DividerBlock where
  toJSON
    = toJSONWithTypeField "divider"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields  = True
    }

divider_ :: Blocks
divider_ = block $ DividerBlock Nothing

divider :: "block_id" :! Text -> Blocks
divider (Arg blockId) = block . DividerBlock $ Just blockId
