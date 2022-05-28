module Slacker.Blocks.Elements.Button
  ( ButtonElement(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ButtonElement
  = ButtonElement
  { text                :: !PlainTextObject
  , action_id           :: !Text
  , url                 :: !(Maybe Text)
  , value               :: !(Maybe Text)
  , style               :: !(Maybe Text)
  , accessibility_label :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Default ButtonElement where
  def
    = ButtonElement
    { text                = ""
    , action_id           = ""
    , url                 = Nothing
    , value               = Nothing
    , style               = Nothing
    , accessibility_label = Nothing
    }

instance Aeson.ToJSON ButtonElement where
  toJSON
    = toJSONWithTypeField "button"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
