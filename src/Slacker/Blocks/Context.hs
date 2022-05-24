{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Context
  ( ContextBlock
  , ContextElement
  , context
  , context_
  , contextImage
  , contextImage_
  , contextText
  ) where

import qualified Data.Aeson as Aeson
import           Data.DList.DNonEmpty (DNonEmpty)
import           Data.Text (Text)
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Elements (ImageElement, TextObject, imageElement_, markdown)
import           Slacker.Blocks.Core (Blocks, block)
import           Slacker.Util (toJSONWithTypeField)

data ContextBlock
  = ContextBlock
  { cbElements :: ContextElements
  , cbBlockId  :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ContextBlock where
  toJSON
    = toJSONWithTypeField "context"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

data ContextElement
  = ContextImage !ImageElement
  | ContextText !TextObject
  | Append ContextElements
  deriving stock (Generic, Show, Eq, Ord)

instance Semigroup ContextElement where
  a <> b = Append $ (ContextElements $ pure a) <> (ContextElements $ pure b)

newtype ContextElements = ContextElements (DNonEmpty ContextElement)
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON, Semigroup)

instance IsString ContextElements where
  fromString = ContextElements . pure . ContextText . markdown . fromString

instance Aeson.ToJSON ContextElement where
  toJSON (ContextImage el) = Aeson.toJSON el
  toJSON (ContextText el)  = Aeson.toJSON el
  toJSON (Append el)       = Aeson.toJSON el

context
  :: "elements" :! ContextElements
  -> "block_id" :? Text
  -> Blocks
context (Arg els) (ArgF mBlockId)
  = block
  $ ContextBlock
  { cbElements = els
  , cbBlockId  = mBlockId
  }

context_ :: ContextElements -> Blocks
context_ els = context ! #elements els ! defaults

contextImage
  :: "image_url" :! Text
  -> "alt_text"  :! Text
  -> ContextElements
contextImage (Arg url) (Arg alt)
  = ContextElements . pure . ContextImage $ imageElement_ url alt

contextImage_
  :: Text
  -> Text
  -> ContextElements
contextImage_ url alt
  = contextImage
  ! #image_url url
  ! #alt_text alt

contextText :: TextObject -> ContextElements
contextText = ContextElements . pure . ContextText
