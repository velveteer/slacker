module Slacker.Blocks.Actions
  ( ActionsBlock(..)
  , ActionsElement(..)
  , ActionsElementTypes
  , defaultActions
  , asAction
  ) where

import qualified Data.Aeson as Aeson
import           Data.DList.DNonEmpty (DNonEmpty(..))
import           Data.Text (Text)
import           Data.WorldPeace
import           GHC.Generics (Generic)

import           Slacker.Blocks.Elements
import           Slacker.Util (toJSONWithTypeField)

data ActionsBlock
  = ActionsBlock
  { elements :: DNonEmpty ActionsElement
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

defaultActions :: DNonEmpty ActionsElement -> ActionsBlock
defaultActions els
  = ActionsBlock
  { elements = els
  , block_id = Nothing
  }

instance Aeson.ToJSON ActionsBlock where
  toJSON
    = toJSONWithTypeField "actions"
    . Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

type ActionsElementTypes
  = '[ ButtonElement
     ]

instance HasButton ActionsElement where
  button = asAction

newtype ActionsElement = ActionsElement (OpenUnion ActionsElementTypes)
  deriving newtype (Aeson.ToJSON)

asAction :: forall a. IsMember a ActionsElementTypes => a -> ActionsElement
asAction = ActionsElement . openUnionLift

