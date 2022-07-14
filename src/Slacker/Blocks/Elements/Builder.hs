{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Elements.Builder
  ( Elements(..)
  , elementsToValues
  ) where

import qualified Data.Aeson as Aeson
import           Data.Kind (Type)
import           GHC.Exts (IsString(..))

import           Slacker.Blocks.Append
import           Slacker.Blocks.Fields
import           Slacker.Blocks.Elements.Button
import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject

data Elements i a where
  Button  :: ButtonElement -> Elements '[ButtonElement] a
  TextObj :: TextObject -> Elements '[TextObject] a
  Fields  :: SectionFields -> Elements '[SectionFields] a
  ImageE  :: ImageElement -> Elements '[ImageElement] a
  EAppend :: Elements as a -> Elements bs b -> Elements (as ++ bs) c

instance IxAppend Elements where
  (>>) = EAppend

instance (i ~ '[Type], a ~ ()) => Aeson.ToJSON (Elements i a) where
  toJSON els = Aeson.toJSON $ elementsToValues els []

instance (i ~ '[TextObject], a ~ ()) => IsString (Elements i a) where
  fromString el = TextObj (fromString el)

instance (i ~ '[TextObject], a ~ ()) => HasText (Elements i a) where
  markdown txt = TextObj (markdownObj txt)
  plaintext txt = TextObj (plaintextObj txt)

instance (i ~ '[ButtonElement], a ~ ()) => HasButton (Elements i a) where
  button el = Button el

instance (i ~ '[ImageElement], a ~ ()) => HasImage ImageElement (Elements i a) where
  image el = ImageE el
  image_ url alt = image $ defaultImage url alt

instance (i ~ '[SectionFields], a ~ ()) => HasFields (Elements i a) where
  fields txts = Fields txts

elementsToValues :: Elements i () -> [Aeson.Value] -> [Aeson.Value]
elementsToValues = go
  where
    go :: Elements i a -> [Aeson.Value] -> [Aeson.Value]
    go (Button e)  = (Aeson.toJSON e :)
    go (ImageE e)  = (Aeson.toJSON e :)
    go (TextObj e) = (Aeson.toJSON e :)
    go (Fields e)  = (Aeson.toJSON e :)
    go (EAppend x y) = go x . go y

