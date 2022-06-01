module Slacker.Blocks.Context
  ( ContextBlock(..)
  , ContextElement(..)
  , ContextElementTypes
  , asContext
  , defaultContext
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ContextBlock
  = ContextBlock
  { elements :: ![ContextElement]
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

defaultContext :: ContextBlock
defaultContext
  = ContextBlock
  { elements = mempty
  , block_id = Nothing
  }

asContext :: forall a. IsMember a ContextElementTypes => a -> ContextElement
asContext = ContextElement . openUnionLift

newtype ContextElement = ContextElement { unContextElements :: OpenUnion ContextElementTypes }
  deriving newtype (Aeson.ToJSON)

instance HasText ContextElement where
  markdown txt = ContextElement . openUnionLift $ markdownObj txt
  plaintext txt = ContextElement . openUnionLift $ plaintextObj txt

instance HasImage ImageElement ContextElement where
  image = ContextElement . openUnionLift
  image_ url alt = image $ defaultImage url alt

type ContextElementTypes
  = '[ ImageElement
     , TextObject
     ]

instance Aeson.ToJSON ContextBlock where
  toJSON
    = toJSONWithTypeField "context"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
