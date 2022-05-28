{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Slacker.Blocks.Elements.Builder
  ( Elements
  , ElementM(..)
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
import           Data.Text (Text)
import           GHC.Exts (IsString(..))

import           Slacker.Blocks.Elements.Button (ButtonElement(..))
import           Slacker.Blocks.Elements.Image (ImageElement(..))
import           Slacker.Blocks.Elements.TextObject

data ElementM a
  = Button ButtonElement a
  | TextObj TextObject a
  | Field TextObject a
  | ImageE ImageElement a
  | forall b. EAppend (ElementM b) (ElementM a)
  | EEmpty a

type Elements = ElementM ()

instance Aeson.ToJSON (ElementM ()) where
  toJSON els = Aeson.toJSON $ elementsToValues els []

instance (a ~ ()) => IsString (ElementM a) where
  fromString el = TextObj (fromString el) ()

elementsToValues :: Elements -> [Aeson.Value] -> [Aeson.Value]
elementsToValues = go
  where
    go :: ElementM b -> [Aeson.Value] -> [Aeson.Value]
    go (Button e _)  = (Aeson.toJSON e :)
    go (ImageE e _)  = (Aeson.toJSON e :)
    go (TextObj e _) = (Aeson.toJSON e :)
    go (Field e _)   = (Aeson.toJSON e :)
    go (EAppend x y) = go x . go y
    go (EEmpty _ )   = id

instance Semigroup (ElementM a) where
  x <> y = EAppend x y

instance Functor ElementM where
  fmap f x = EAppend x (EEmpty (f (elementValue x)))

instance Applicative ElementM where
  pure x = EEmpty x
  {-# INLINE pure #-}
  (<*>) x y = EAppend (EAppend x y) (EEmpty (elementValue x (elementValue y)))
  {-# INLINE (<*>) #-}
  (*>) = EAppend
  {-# INLINE (*>) #-}

instance Monad ElementM where
  return = pure
  {-# INLINE return #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  b >>= f = EAppend b (f (elementValue b))
  {-# INLINE (>>=) #-}

elementValue :: ElementM a -> a
elementValue = \case
  Button _ x  -> x
  ImageE _ x  -> x
  TextObj _ x -> x
  Field _ x   -> x
  EAppend _ x -> elementValue x
  EEmpty x    -> x

button :: ButtonElement -> Elements
button el = Button el ()

button_ :: Text -> Text -> Elements
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


imageE :: ImageElement -> Elements
imageE i = ImageE i ()

imageE_ :: Text -> Text -> Elements
imageE_ url alt
  = ImageE
  (ImageElement
  { alt_text  = alt
  , image_url = url
  }) ()

markdown :: Text -> Elements
markdown txt = TextObj (markdownObj txt) ()

plaintext :: Text -> Elements
plaintext txt = TextObj (plaintextObj txt) ()

field :: TextObject -> Elements
field txt = Field txt ()

