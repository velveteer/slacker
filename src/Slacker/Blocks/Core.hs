module Slacker.Blocks.Core
  ( Blocks
  , block
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.DList.DNonEmpty as DL

newtype Blocks = Blocks (DL.DNonEmpty Aeson.Value)
  deriving newtype (Eq, Ord, Show, Aeson.ToJSON, Semigroup)

block :: Aeson.ToJSON a => a -> Blocks
block = Blocks . pure . Aeson.toJSON
