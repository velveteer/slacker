{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Slacker.Blocks.Builder
  ( Blocks
  , actions
  , actions_
  , context
  , context_
  , divider
  , divider_
  , section
  , section_
  , header
  , header_
  , image
  , image_
  , withBlockId
  ) where

import qualified Data.Aeson as Aeson
import           Data.Default (Default(..))
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)

import           Slacker.Blocks.Actions (ActionsBlock(..))
import           Slacker.Blocks.Context (ContextBlock(..), ContextElement(..))
import           Slacker.Blocks.Divider (DividerBlock(..))
import           Slacker.Blocks.Elements
import           Slacker.Blocks.Header (HeaderBlock(..))
import           Slacker.Blocks.Image (ImageBlock(..))
import           Slacker.Blocks.Section (SectionBlock(..), asAccessory)

data BlockM a
  = Section SectionBlock a
  | Header HeaderBlock a
  | Context ContextBlock a
  | Divider DividerBlock a
  | Image ImageBlock a
  | Actions ActionsBlock a
  | forall b. Append (BlockM b) (BlockM a)
  | Empty a

type Blocks = BlockM ()

instance Aeson.ToJSON (BlockM ()) where
  toJSON bs = Aeson.toJSON $ blocksToValues bs []

blocksToValues :: Blocks -> [Aeson.Value] -> [Aeson.Value]
blocksToValues = go
  where
    go :: BlockM b -> [Aeson.Value] -> [Aeson.Value]
    go (Section b _) = (Aeson.toJSON b :)
    go (Header b _)  = (Aeson.toJSON b :)
    go (Context b _) = (Aeson.toJSON b :)
    go (Actions b _) = (Aeson.toJSON b :)
    go (Divider b _) = (Aeson.toJSON b :)
    go (Image b _)   = (Aeson.toJSON b :)
    go (Append x y)  = go x . go y
    go (Empty _ )    = id

instance Semigroup (BlockM a) where
  x <> y = Append x y

instance Functor BlockM where
  fmap f x = Append x (Empty (f (blockValue x)))

instance Applicative BlockM where
  pure x = Empty x
  {-# INLINE pure #-}
  (<*>) x y = Append (Append x y) (Empty (blockValue x (blockValue y)))
  {-# INLINE (<*>) #-}
  (*>) = Append
  {-# INLINE (*>) #-}

instance Monad BlockM where
  return = pure
  {-# INLINE return #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  b >>= f = Append b (f (blockValue b))
  {-# INLINE (>>=) #-}

blockValue :: BlockM a -> a
blockValue = \case
  Section _ x -> x
  Header _ x  -> x
  Context _ x -> x
  Actions _ x -> x
  Divider _ x -> x
  Image _ x   -> x
  Append _ x  -> blockValue x
  Empty x     -> x

withBlockId :: Text -> Blocks -> Blocks
withBlockId bid = \case
  Section b _ -> Section (b { block_id = Just bid }) ()
  Header b _  -> Header (b { block_id = Just bid }) ()
  Context b _ -> Context (b { block_id = Just bid }) ()
  Actions b _ -> Actions (b { block_id = Just bid }) ()
  Divider b _ -> Divider (b { block_id = Just bid }) ()
  Image b _   -> Image (b { block_id = Just bid }) ()
  Append a b  -> Append a (withBlockId bid b)
  Empty b     -> Empty b

section :: SectionBlock -> Blocks
section s = Section s ()

section_ :: Elements -> Blocks
section_ els = Section (go els def) ()
  where
    go :: ElementM b -> SectionBlock -> SectionBlock
    go (TextObj t _) = \b -> b{ text = t }
    go (Button bb _) = \b -> b{ accessory = Just $ asAccessory bb }
    go (ImageE i _) = \b -> b{ accessory = Just $ asAccessory i }
    go (Field f _)   = \b -> b{ fields = Just $ maybe (pure f) (f NE.<|) (fields b) }
    go (EAppend x y) = go x . go y
    go (EEmpty _) = id

context :: ContextBlock -> Blocks
context c = Context c ()

context_ :: Elements -> Blocks
context_ els = Context (go els def) ()
  where
    go :: ElementM b -> ContextBlock -> ContextBlock
    go (TextObj t _) = \b -> b{ elements = ContextText t : elements (b :: ContextBlock) }
    go (ImageE i _) = \b -> b{ elements = ContextImage i : elements (b :: ContextBlock) }
    go (EAppend x y) = go x . go y
    -- TODO Can the compiler enforce this?
    go (Field _ _) = id
    go (Button _ _) = id
    go (EEmpty _) = id

header :: HeaderBlock -> Blocks
header h = Header h ()

header_ :: Text -> Blocks
header_ txt
  = Header
  (HeaderBlock
  { block_id = Nothing
  , text     = plaintext_ txt
  }) ()

image :: ImageBlock -> Blocks
image el = Image el ()

image_ :: Text -> Text -> Blocks
image_ url alt
  = Image
  (ImageBlock
  { title     = Nothing
  , block_id  = Nothing
  , image_url = url
  , alt_text  = alt
  }) ()

actions :: ActionsBlock -> Blocks
actions a = Actions a ()

actions_ :: Elements -> Blocks
actions_ els = Actions (ActionsBlock els Nothing) ()

divider :: DividerBlock -> Blocks
divider d = Divider d ()

divider_ :: Blocks
divider_ = Divider (DividerBlock Nothing) ()
