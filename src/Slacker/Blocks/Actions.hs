module Slacker.Blocks.Actions
  ( ActionsBlock(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements
import           Slacker.Util (toJSONWithTypeField)

data ActionsBlock
  = ActionsBlock
  { elements :: Elements
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

instance Default ActionsBlock where
  def
    = ActionsBlock
    { elements = EEmpty ()
    , block_id = Nothing
    }

instance Aeson.ToJSON ActionsBlock where
  toJSON
    = toJSONWithTypeField "actions"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
