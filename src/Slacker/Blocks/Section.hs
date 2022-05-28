module Slacker.Blocks.Section
  ( SectionBlock(..)
  , asAccessory
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements
import           Slacker.Util (toJSONWithTypeField)

data SectionBlock
  = SectionBlock
  { text      :: !TextObject
  , block_id  :: !(Maybe Text)
  , fields    :: !(Maybe (NonEmpty TextObject))
  , accessory :: !(Maybe (OpenUnion SectionAccessory))
  } deriving stock (Generic)

type SectionAccessory
  = '[ ButtonElement
     , ImageElement
     ]

instance Default SectionBlock where
  def
    = SectionBlock
    { text      = ""
    , block_id  = Nothing
    , fields    = Nothing
    , accessory = Nothing
    }

instance Aeson.ToJSON SectionBlock where
  toJSON
    = toJSONWithTypeField "section"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

asAccessory :: forall a as. IsMember a as => a -> OpenUnion as
asAccessory = openUnionLift
