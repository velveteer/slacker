{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Section
  ( SectionBlock(..)
  , SectionAccessory(..)
  , SectionAccessoryTypes
  , SectionFields(..)
  , HasFields(..)
  , asAccessory
  , defaultSection
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Exts (IsList(..))
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.Button
import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data SectionBlock
  = SectionBlock
  { text      :: !TextObject
  , block_id  :: !(Maybe Text)
  , fields    :: !(Maybe SectionFields)
  , accessory :: !(Maybe SectionAccessory)
  } deriving stock (Generic)

defaultSection :: TextObject -> SectionBlock
defaultSection txt
  = SectionBlock
  { text      = txt
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

type SectionAccessoryTypes
  = '[ ButtonElement
     , ImageElement
     ]

newtype SectionAccessory
  = SectionAccessory
  { unSectionAccessory :: OpenUnion SectionAccessoryTypes
  } deriving newtype (Aeson.ToJSON)

instance HasButton SectionAccessory where
  button = asAccessory

instance HasImage ImageElement SectionAccessory where
  image = asAccessory
  image_ url alt = image $ defaultImage url alt

asAccessory :: forall a. IsMember a SectionAccessoryTypes => a -> SectionAccessory
asAccessory = SectionAccessory . openUnionLift

newtype SectionFields = SectionFields [TextObject]
  deriving newtype (Aeson.ToJSON)

instance IsList SectionFields where
  type Item SectionFields  = TextObject
  fromList                 = SectionFields
  toList (SectionFields l) = l

class HasFields a where
  fields :: [TextObject] -> a

instance HasFields SectionFields where
  fields = SectionFields

