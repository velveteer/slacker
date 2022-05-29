{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Data.WorldPeace

import           Slacker.Blocks.Actions (ActionsBlock(..), ActionsElements, asAction)
import qualified Slacker.Blocks.Actions as Actions
import           Slacker.Blocks.Context (ContextBlock(..), ContextElements, asContext)
import qualified Slacker.Blocks.Context as Context
import           Slacker.Blocks.Divider (DividerBlock(..))
import qualified Slacker.Blocks.Divider as Divider
import           Slacker.Blocks.Elements
import           Slacker.Blocks.Header (HeaderBlock(..))
import qualified Slacker.Blocks.Header as Header
import           Slacker.Blocks.Image (ImageBlock(..))
import qualified Slacker.Blocks.Image as Image
import           Slacker.Blocks.Section (SectionAccessory, SectionBlock(..), asAccessory)
import qualified Slacker.Blocks.Section as Section

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
  Section b _ -> Section (b { Section.block_id = Just bid }) ()
  Header b _  -> Header (b { Header.block_id = Just bid }) ()
  Context b _ -> Context (b { Context.block_id = Just bid }) ()
  Actions b _ -> Actions (b { Actions.block_id = Just bid }) ()
  Divider b _ -> Divider (b { Divider.block_id = Just bid }) ()
  Image b _   -> Image (b { Image.block_id = Just bid }) ()
  Append a b  -> Append a (withBlockId bid b)
  Empty b     -> Empty b

section :: SectionBlock -> Blocks
section s = Section s ()

section_ :: (Contains i (SectionField ': SectionAccessory)) => Elements i -> Blocks
section_ els = Section (go els def) ()
  where
    go :: ElementM i b -> SectionBlock -> SectionBlock
    go (TextObj t _) = \b -> b{ Section.text = t }
    go (Button bb _) = \b -> b{ accessory = Just $ asAccessory bb }
    go (ImageE i _) = \b -> b{ accessory = Just $ asAccessory i }
    go (Field f _)   = \b -> b{ fields = Just $ maybe (pure f) (f NE.<|) (fields b) }
    -- Last element of the do block gets to be the lone accessory
    go (EAppend x y) = go y . go x
    go (EEmpty _) = id

context :: ContextBlock -> Blocks
context c = Context c ()

context_ :: (Contains i ContextElements) => Elements i -> Blocks
context_ els = Context (go els def) ()
  where
    go :: ElementM i b -> ContextBlock -> ContextBlock
    go (TextObj t _) = \b -> b{ Context.elements = asContext t : Context.elements b }
    go (ImageE i _) = \b -> b{ Context.elements = asContext i : Context.elements b }
    go (EAppend x y) = go x . go y
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

actions_ :: Contains i ActionsElements => Elements i -> Blocks
actions_ els = Actions (go els def) ()
  where
    go :: ElementM i b -> ActionsBlock -> ActionsBlock
    go (Button i _)  = \b -> b{ Actions.elements = asAction i : Actions.elements b }
    go (EAppend x y) = go y . go x
    go (ImageE _ _)  = id
    go (Field _ _)   = id
    go (EEmpty _)    = id
    go (TextObj _ _) = id

divider :: DividerBlock -> Blocks
divider d = Divider d ()

divider_ :: Blocks
divider_ = Divider (DividerBlock Nothing) ()
