module Slacker.Events.AppMention
  ( AppMention(..)
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
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON AppMention where
  parseJSON = genericFromJSONWithType "AppMention" "app_mention"
