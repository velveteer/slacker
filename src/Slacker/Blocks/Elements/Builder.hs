{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Elements.Builder
  ( Elements
  , ElementM(..)
  , IxMonad(..)
  , SectionField
  , elementsToValues
  , button
  , button_
  , imageE
  , imageE_
  , markdown
  , plaintext
  , field
  ) where

import qualified Data.Aeson as Aeson
import           Data.Kind (Type)
import           Data.Text (Text)
import           GHC.Exts (IsString(..))

import           Slacker.Blocks.Elements.Button (ButtonElement(..))
import           Slacker.Blocks.Elements.Image (ImageElement(..))
import           Slacker.Blocks.Elements.TextObject

type family (++) (xs :: [Type]) (ys :: [Type]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data ElementM i a where
  Button  :: ButtonElement -> a -> ElementM '[ButtonElement] a
  TextObj :: TextObject -> a -> ElementM '[TextObject] a
  Field   :: SectionField -> a -> ElementM '[SectionField] a
  ImageE  :: ImageElement -> a -> ElementM '[ImageElement] a
  EAppend :: (ElementM bs b) -> (ElementM as a) -> ElementM (bs ++ as) a
  EEmpty  :: a -> ElementM '[] a

type Elements i = ElementM i ()

instance (i ~ '[Type]) => Aeson.ToJSON (ElementM i ()) where
  toJSON els = Aeson.toJSON $ elementsToValues els []

instance (i ~ '[TextObject], a ~ ()) => IsString (ElementM i a) where
  fromString el = TextObj (fromString el) ()

elementsToValues :: Elements i -> [Aeson.Value] -> [Aeson.Value]
elementsToValues = go
  where
    go :: ElementM i b -> [Aeson.Value] -> [Aeson.Value]
    go (Button e _)  = (Aeson.toJSON e :)
    go (ImageE e _)  = (Aeson.toJSON e :)
    go (TextObj e _) = (Aeson.toJSON e :)
    go (Field e _)   = (Aeson.toJSON e :)
    go (EAppend x y) = go x . go y
    go (EEmpty _ )   = id

class IxMonad (m :: k -> Type -> Type) where
  type Unit m :: k
  type Plus m (i :: k) (j :: k) :: k
  return :: a -> m (Unit m) a
  (>>=)  :: m i a -> (a -> m j b) -> m (Plus m i j) b
  (>>)   :: m i a -> m j b -> m (Plus m i j) b

instance IxMonad ElementM where
  type Unit ElementM = '[]
  type Plus ElementM i j = i ++ j
  return  = EEmpty
  b >>= f = EAppend b (f (elementValue b))
  (>>)    = EAppend

elementValue :: ElementM i a -> a
elementValue = \case
  Button _ x  -> x
  ImageE _ x  -> x
  TextObj _ x -> x
  Field _ x   -> x
  EAppend _ x -> elementValue x
  EEmpty x    -> x

button :: ButtonElement -> Elements '[ButtonElement]
button el = Button el ()

button_ :: Text -> Text -> Elements '[ButtonElement]
button_ txt actId
  = Button
  (ButtonElement
  { text                = plaintext_ txt
  , action_id           = actId
  , url                 = Nothing
  , value               = Nothing
  , style               = Nothing
  , accessibility_label = Nothing
  }) ()

imageE :: ImageElement -> Elements '[ImageElement]
imageE i = ImageE i ()

imageE_ :: Text -> Text -> Elements '[ImageElement]
imageE_ url alt
  = ImageE
  (ImageElement
  { alt_text  = alt
  , image_url = url
  }) ()

markdown :: Text -> Elements '[TextObject]
markdown txt = TextObj (markdownObj txt) ()

plaintext :: Text -> Elements '[TextObject]
plaintext txt = TextObj (plaintextObj txt) ()

newtype SectionField = SectionField TextObject
  deriving newtype (Aeson.ToJSON)

field :: TextObject -> Elements '[SectionField]
field txt = Field (SectionField txt) ()

