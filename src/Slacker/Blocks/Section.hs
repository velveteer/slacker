{-# LANGUAGE FlexibleInstances #-}

module Slacker.Blocks.Section
  ( SectionBlock(..)
  , SectionAccessory(..)
  , SectionAccessoryTypes
  , SectionField(..)
  , HasField(..)
  , asAccessory
  , defaultSection
  ) where

import qualified Data.Aeson as Aeson
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.Button
import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data SectionBlock
  = SectionBlock
  { text      :: !TextObject
  , block_id  :: !(Maybe Text)
  , fields    :: !(Maybe (NonEmpty SectionField))
  , accessory :: !SectionAccessory
  } deriving stock (Generic)

defaultSection :: TextObject -> SectionBlock
defaultSection txt
  = SectionBlock
  { text      = txt
  , block_id  = Nothing
  , fields    = Nothing
  , accessory = SectionAccessory Nothing
  }

instance Aeson.ToJSON SectionBlock where
  toJSON
    = toJSONWithTypeField "section"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

type SectionAccessoryTypes
  = '[ ButtonElement
     , ImageElement
     ]

newtype SectionAccessory
  = SectionAccessory
  { unSectionAccessory :: Maybe (OpenUnion SectionAccessoryTypes)
  } deriving newtype (Aeson.ToJSON)

instance HasButton SectionAccessory where
  button = asAccessory

instance HasImage ImageElement SectionAccessory where
  image = asAccessory
  image_ url alt = image $ defaultImage url alt

asAccessory :: forall a. IsMember a SectionAccessoryTypes => a -> SectionAccessory
asAccessory = SectionAccessory . pure . openUnionLift

newtype SectionField = SectionField TextObject
  deriving newtype (Aeson.ToJSON)

class HasField a where
  field :: TextObject -> a

instance HasField SectionField where
  field = SectionField

