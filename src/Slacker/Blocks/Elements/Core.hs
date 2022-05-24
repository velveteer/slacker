module Slacker.Blocks.Elements.Core
  ( Elements(..)
  , element
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.DList.DNonEmpty as DL

newtype Elements = Elements { unElements :: DL.DNonEmpty Aeson.Value }
  deriving newtype (Eq, Ord, Show, Aeson.ToJSON, Semigroup)

element :: Aeson.ToJSON a => a -> Elements
element = Elements . pure . Aeson.toJSON
