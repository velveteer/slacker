module Slacker.Blocks.Context
  ( ContextBlock(..)
  , ContextElement(..)
  , ContextElementTypes
  , asContext
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements (ImageElement, TextObject)
import           Slacker.Util (toJSONWithTypeField)

data ContextBlock
  = ContextBlock
  { elements :: ![ContextElement]
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

newtype ContextElement = ContextElement { unContextElements :: OpenUnion ContextElementTypes }
  deriving newtype (Aeson.ToJSON)

type ContextElementTypes
  = '[ ImageElement
     , TextObject
     ]

asContext :: forall a. IsMember a ContextElementTypes => a -> ContextElement
asContext = ContextElement . openUnionLift

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
