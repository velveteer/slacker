module Slacker.Blocks.Actions
  ( ActionsBlock(..)
  , ActionsElement(..)
  , ActionsElements(..)
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
  { elements :: !ActionsElements
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

newtype ActionsElements = ActionsElements (DNonEmpty ActionsElement)
  deriving newtype (Aeson.ToJSON, Semigroup)

instance HasButton ActionsElements where
  button = asAction

defaultActions :: ActionsElements -> ActionsBlock
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

newtype ActionsElement = ActionsElement (OpenUnion ActionsElementTypes)
  deriving newtype (Aeson.ToJSON)

asAction :: forall a. IsMember a ActionsElementTypes => a -> ActionsElements
asAction = ActionsElements . pure . ActionsElement . openUnionLift

