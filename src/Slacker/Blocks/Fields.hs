{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Fields
  ( HasFields(..)
  , SectionFields(..)
  , field
  ) where

import qualified Data.Aeson as Aeson
import           Data.DList.DNonEmpty (DNonEmpty(..))
import           GHC.Exts (IsString(..))

import           Slacker.Blocks.Elements.TextObject

class HasFields a where
  fields :: SectionFields -> a

instance HasFields SectionFields where
  fields = id

newtype SectionFields = SectionFields (DNonEmpty TextObject)
  deriving newtype (Aeson.ToJSON, Semigroup)

instance IsString SectionFields where
  fromString s = SectionFields . pure $ fromString s

field :: TextObject -> SectionFields
field = SectionFields . pure
