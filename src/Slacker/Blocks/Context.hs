module Slacker.Blocks.Context
  ( ContextBlock(..)
  , ContextElements
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
  { elements :: ![OpenUnion ContextElements]
  , block_id :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

type ContextElements = '[ImageElement, TextObject]

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

asContext :: forall a as. IsMember a as => a -> OpenUnion as
asContext = openUnionLift
