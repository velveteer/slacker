module Slacker.Blocks.Context
  ( ContextBlock(..)
  , ContextElements(..)
  , ContextElement
  , ContextElementTypes
  , asContext
  , defaultContext
  , emptyContext
  ) where

import qualified Data.Aeson as Aeson
import           Data.DList (DList(..))
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject
import           Slacker.Util (toJSONWithTypeField)

data ContextBlock
  = ContextBlock
  { elements :: !ContextElements
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

newtype ContextElements = ContextElements (DList ContextElement)
  deriving newtype (Aeson.ToJSON, Semigroup)

instance IsString ContextElements where
  fromString s = ContextElements . pure $ fromString s

defaultContext :: ContextElements -> ContextBlock
defaultContext els
  = ContextBlock
  { elements = els
  , block_id = Nothing
  }

emptyContext :: ContextBlock
emptyContext
  = ContextBlock
  { elements = ContextElements mempty
  , block_id = Nothing
  }

asContext :: forall a. IsMember a ContextElementTypes => a -> ContextElements
asContext = ContextElements . pure . ContextElement . openUnionLift

newtype ContextElement = ContextElement (OpenUnion ContextElementTypes)
  deriving newtype (Aeson.ToJSON)

instance IsString ContextElement where
  fromString s = ContextElement . openUnionLift $ markdownObj (fromString s)

instance HasText ContextElements where
  markdown txt = ContextElements . pure . ContextElement . openUnionLift $ markdownObj txt
  plaintext txt = ContextElements . pure . ContextElement . openUnionLift $ plaintextObj txt

instance HasImage ImageElement ContextElements where
  image = ContextElements . pure . ContextElement . openUnionLift
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
