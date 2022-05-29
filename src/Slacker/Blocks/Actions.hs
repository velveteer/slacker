module Slacker.Blocks.Actions
  ( ActionsBlock(..)
  , ActionsElements
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
  { elements :: [OpenUnion ActionsElements]
  , block_id :: !(Maybe Text)
  } deriving stock (Generic)

type ActionsElements = '[ButtonElement]

asAction :: forall a as. IsMember a as => a -> OpenUnion as
asAction = openUnionLift

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
