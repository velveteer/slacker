module Slacker.Blocks.Actions
  ( ActionsBlock(..)
  , ActionsElement(..)
  , ActionsElementTypes
  , asAction
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements
import           Slacker.Util (toJSONWithTypeField)

data ActionsBlock
  = ActionsBlock
  { elements :: [ActionsElement]
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

type ActionsElementTypes
  = '[ ButtonElement
     ]

newtype ActionsElement = ActionsElement { unActionElement :: OpenUnion ActionsElementTypes }
  deriving newtype (Aeson.ToJSON)

asAction :: forall a. IsMember a ActionsElementTypes => a -> ActionsElement
asAction = ActionsElement . openUnionLift

instance Default ActionsBlock where
  def
    = ActionsBlock
    { elements = []
    , block_id = Nothing
    }

instance Aeson.ToJSON ActionsBlock where
  toJSON
    = toJSONWithTypeField "actions"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }
