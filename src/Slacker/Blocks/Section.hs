module Slacker.Blocks.Section
  ( SectionBlock(..)
  , SectionAccessory(..)
  , SectionAccessoryTypes
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
  , fields    :: !(Maybe (NonEmpty SectionField))
  , accessory :: !(Maybe SectionAccessory)
  } deriving stock (Generic)

type SectionAccessoryTypes
  = '[ ButtonElement
     , ImageElement
     ]

newtype SectionAccessory = SectionAccessory { unSectionAccessory :: OpenUnion SectionAccessoryTypes }
  deriving newtype (Aeson.ToJSON)

asAccessory :: forall a. IsMember a SectionAccessoryTypes => a -> SectionAccessory
asAccessory = SectionAccessory . openUnionLift

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
