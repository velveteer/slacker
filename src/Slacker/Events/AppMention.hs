module Slacker.Events.AppMention
  ( AppMention(..)
  , Edited(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Util (genericFromJSONWithType)

data AppMention
  = AppMention
  { user     :: !Text
  , text     :: !Text
  , ts       :: !Text
  , channel  :: !Text
  , event_ts :: !Text
  , edited   :: !(Maybe Edited)
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON AppMention where
  parseJSON = genericFromJSONWithType "AppMention" "app_mention"

data Edited
  = Edited
  { eUser :: !Text
  , eTs   :: !Text
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Edited where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 1
    }
