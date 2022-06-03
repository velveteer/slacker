{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Elements.Builder
  ( Elements
  , ElementM(..)
  , elementsToValues
  ) where

import qualified Data.Aeson as Aeson
import           Data.Kind (Type)
import           GHC.Exts (IsString(..))

import           Slacker.Blocks.Append
import           Slacker.Blocks.Section
import           Slacker.Blocks.Elements.Button
import           Slacker.Blocks.Elements.Image
import           Slacker.Blocks.Elements.TextObject

data ElementM i a where
  Button  :: ButtonElement -> a -> ElementM '[ButtonElement] a
  TextObj :: TextObject -> a -> ElementM '[TextObject] a
  Fields  :: SectionFields -> a -> ElementM '[SectionFields] a
  ImageE  :: ImageElement -> a -> ElementM '[ImageElement] a
  EAppend :: ElementM as b -> ElementM bs a -> ElementM (as ++ bs) a

type Elements i = ElementM i ()

instance IxAppend ElementM where
  type Unit ElementM = '[]
  type Plus ElementM i j = i ++ j
  (>>) = EAppend

instance (i ~ '[Type]) => Aeson.ToJSON (ElementM i ()) where
  toJSON els = Aeson.toJSON $ elementsToValues els []

instance (i ~ '[TextObject], a ~ ()) => IsString (ElementM i a) where
  fromString el = TextObj (fromString el) ()

instance (i ~ '[TextObject], a ~ ()) => HasText (ElementM i a) where
  markdown txt = TextObj (markdownObj txt) ()
  plaintext txt = TextObj (plaintextObj txt) ()

instance (i ~ '[ButtonElement], a ~ ()) => HasButton (ElementM i a) where
  button el = Button el ()

instance (i ~ '[ImageElement], a ~ ()) => HasImage ImageElement (ElementM i a) where
  image el = ImageE el ()
  image_ url alt = image $ defaultImage url alt

instance (i ~ '[SectionFields], a ~ ()) => HasFields (ElementM i a) where
  fields txts = Fields txts ()

elementsToValues :: Elements i -> [Aeson.Value] -> [Aeson.Value]
elementsToValues = go
  where
    go :: ElementM i b -> [Aeson.Value] -> [Aeson.Value]
    go (Button e _)  = (Aeson.toJSON e :)
    go (ImageE e _)  = (Aeson.toJSON e :)
    go (TextObj e _) = (Aeson.toJSON e :)
    go (Fields e _)  = (Aeson.toJSON e :)
    go (EAppend x y) = go x . go y

