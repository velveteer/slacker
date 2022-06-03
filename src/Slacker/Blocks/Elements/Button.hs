module Slacker.Blocks.Elements.Button
  ( ButtonElement(..)
  , HasButton(..)
  , defaultButton
  ) where

import qualified Data.Aeson as Aeson
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

class HasButton a where
  button :: ButtonElement -> a
  button_ :: Text -> Text -> a
  button_ txt actId = button $ defaultButton txt actId

instance HasButton ButtonElement where
  button = id

defaultButton :: Text -> Text -> ButtonElement
defaultButton txt actId
  = ButtonElement
  { text                = plaintext_ txt
  , action_id           = actId
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
