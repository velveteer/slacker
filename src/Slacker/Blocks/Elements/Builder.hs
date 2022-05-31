{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Elements.Builder
  ( Elements
  , ElementM(..)
  , (*>>)
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

import           Slacker.Blocks.Append
import           Slacker.Blocks.Elements.Button (ButtonElement(..))
import           Slacker.Blocks.Elements.Image (ImageElement(..))
import           Slacker.Blocks.Elements.TextObject

data ElementM i a where
  Button  :: ButtonElement -> a -> ElementM '[ButtonElement] a
  TextObj :: TextObject -> a -> ElementM '[TextObject] a
  Field   :: SectionField -> a -> ElementM '[SectionField] a
  ImageE  :: ImageElement -> a -> ElementM '[ImageElement] a
  EAppend :: ElementM as b -> ElementM bs a -> ElementM (as ++ bs) a

type Elements i = ElementM i ()

-- | Build up elements in sequence using this operator.
-- Alternatively, enable QualifiedDo and use do syntax to sequence elements.
-- The order of elements will matter for most layout blocks.
(*>>) :: ElementM as b -> ElementM bs a -> ElementM (as ++ bs) a
(*>>) = EAppend

instance IxAppend ElementM where
  type Unit ElementM = '[]
  type Plus ElementM i j = i ++ j
  (>>) = EAppend

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

