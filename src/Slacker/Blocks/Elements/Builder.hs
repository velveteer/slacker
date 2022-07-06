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

data Elements i where
  Button  :: ButtonElement -> Elements '[ButtonElement]
  TextObj :: TextObject -> Elements '[TextObject]
  Fields  :: SectionFields -> Elements '[SectionFields]
  ImageE  :: ImageElement -> Elements '[ImageElement]
  EAppend :: Elements as -> Elements bs -> Elements (as ++ bs)

instance IxAppend Elements where
  (>>) = EAppend

instance (i ~ '[Type]) => Aeson.ToJSON (Elements i) where
  toJSON els = Aeson.toJSON $ elementsToValues els []

instance (i ~ '[TextObject]) => IsString (Elements i) where
  fromString el = TextObj (fromString el)

instance (i ~ '[TextObject]) => HasText (Elements i) where
  markdown txt = TextObj (markdownObj txt)
  plaintext txt = TextObj (plaintextObj txt)

instance (i ~ '[ButtonElement]) => HasButton (Elements i) where
  button el = Button el

instance (i ~ '[ImageElement]) => HasImage ImageElement (Elements i) where
  image el = ImageE el
  image_ url alt = image $ defaultImage url alt

instance (i ~ '[SectionFields]) => HasFields (Elements i) where
  fields txts = Fields txts

elementsToValues :: Elements i -> [Aeson.Value] -> [Aeson.Value]
elementsToValues = go
  where
    go :: Elements i -> [Aeson.Value] -> [Aeson.Value]
    go (Button e)  = (Aeson.toJSON e :)
    go (ImageE e)  = (Aeson.toJSON e :)
    go (TextObj e) = (Aeson.toJSON e :)
    go (Fields e)  = (Aeson.toJSON e :)
    go (EAppend x y) = go x . go y

