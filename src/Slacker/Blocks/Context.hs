module Slacker.Blocks.Context
  ( ContextBlock(..)
  , ContextElements(..)
  , ContextElement
  , ContextElementTypes
  , asContext
  , defaultContext
  ) where

import qualified Data.Aeson as Aeson
import           Data.DList.DNonEmpty (DNonEmpty(..))
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

newtype ContextElements = ContextElements (DNonEmpty ContextElement)
  deriving newtype (Aeson.ToJSON, Semigroup)

instance IsString ContextElements where
  fromString s = asContext (fromString s :: TextObject)

defaultContext :: ContextElements -> ContextBlock
defaultContext els
  = ContextBlock
  { elements = els
  , block_id = Nothing
  }

asContext :: forall a. IsMember a ContextElementTypes => a -> ContextElements
asContext = ContextElements . pure . ContextElement . openUnionLift

newtype ContextElement = ContextElement (OpenUnion ContextElementTypes)
  deriving newtype (Aeson.ToJSON)

instance HasText ContextElements where
  markdown txt = asContext $ markdownObj txt
  plaintext txt = asContext $ plaintextObj txt

instance HasImage ImageElement ContextElements where
  image = asContext
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
