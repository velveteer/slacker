module Slacker.Blocks.Context
  ( ContextBlock(..)
  , ContextElement(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements (ImageElement, TextObject)
import           Slacker.Util (toJSONWithTypeField)

data ContextBlock
  = ContextBlock
  { elements :: !ContextElements
  , block_id :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ContextBlock where
  toJSON
    = toJSONWithTypeField "context"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

instance Default ContextBlock where
  def
    = ContextBlock
    { elements = mempty
    , block_id = Nothing
    }

data ContextElement
  = ContextImage !ImageElement
  | ContextText !TextObject
  deriving stock (Generic, Show, Eq, Ord)

type ContextElements = [ContextElement]

instance Aeson.ToJSON ContextElement where
  toJSON (ContextImage el) = Aeson.toJSON el
  toJSON (ContextText el)  = Aeson.toJSON el
