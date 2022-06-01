{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-redundant-constraints #-}

module Slacker.Blocks.Builder
  ( Blocks
  , AllBlocks
  , blocksToUnion
  , blocksToValues
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
  ) where

import qualified Data.Aeson as Aeson
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.WorldPeace

import           Slacker.Blocks.Actions
import qualified Slacker.Blocks.Actions as Actions
import           Slacker.Blocks.Append
import           Slacker.Blocks.Context
import qualified Slacker.Blocks.Context as Context
import           Slacker.Blocks.Divider
import           Slacker.Blocks.Elements
import           Slacker.Blocks.Header
import           Slacker.Blocks.Image
import           Slacker.Blocks.Section
import qualified Slacker.Blocks.Section as Section

data BlockM i a where
  Section :: SectionBlock -> a -> BlockM '[SectionBlock] a
  Header :: HeaderBlock -> a -> BlockM '[HeaderBlock] a
  Context :: ContextBlock -> a -> BlockM '[ContextBlock] a
  Divider :: DividerBlock -> a -> BlockM '[DividerBlock] a
  Image :: ImageBlock -> a -> BlockM '[ImageBlock] a
  Actions :: ActionsBlock -> a -> BlockM '[ActionsBlock] a
  Append :: BlockM as b -> BlockM bs a -> BlockM (as ++ bs) a

type Blocks i = BlockM i ()

instance IxAppend BlockM where
  type Unit BlockM = '[]
  type Plus BlockM i j = i ++ j
  (>>) = Append

instance (i ~ '[Type]) => Aeson.ToJSON (BlockM i ()) where
  toJSON bs = Aeson.toJSON $ blocksToValues bs []

instance (i ~ '[ImageBlock], a ~ ()) => HasImage ImageBlock (BlockM i a) where
  image el = Image el ()
  image_ url alt = image $ defaultImageBlock url alt

type AllBlocks
  = '[ SectionBlock
     , HeaderBlock
     , ContextBlock
     , DividerBlock
     , ImageBlock
     , ActionsBlock
     ]

blocksToUnion :: Blocks i -> [OpenUnion AllBlocks] -> [OpenUnion AllBlocks]
blocksToUnion = go
  where
    go :: BlockM i b -> [OpenUnion AllBlocks] -> [OpenUnion AllBlocks]
    go (Section b _) = (openUnionLift b :)
    go (Header b _)  = (openUnionLift b :)
    go (Context b _) = (openUnionLift b :)
    go (Actions b _) = (openUnionLift b :)
    go (Divider b _) = (openUnionLift b :)
    go (Image b _)   = (openUnionLift b :)
    go (Append x y)  = go x . go y

blocksToValues :: Blocks i -> [Aeson.Value] -> [Aeson.Value]
blocksToValues = go
  where
    go :: BlockM i b -> [Aeson.Value] -> [Aeson.Value]
    go (Section b _) = (Aeson.toJSON b :)
    go (Header b _)  = (Aeson.toJSON b :)
    go (Context b _) = (Aeson.toJSON b :)
    go (Actions b _) = (Aeson.toJSON b :)
    go (Divider b _) = (Aeson.toJSON b :)
    go (Image b _)   = (Aeson.toJSON b :)
    go (Append x y)  = go x . go y

section :: SectionBlock -> Blocks '[SectionBlock]
section s = Section s ()

section_
  :: (Contains i (TextObject ': SectionFields ': SectionAccessoryTypes))
  => Elements i
  -> Blocks '[SectionBlock]
section_ els = Section (go els (defaultSection "")) ()
  where
    go :: ElementM i b -> SectionBlock -> SectionBlock
    go (TextObj t _) = \b -> b{ Section.text = t }
    go (Button bb _) = \b -> b{ accessory = Just $ asAccessory bb }
    go (ImageE i _)  = \b -> b{ accessory = Just $ asAccessory i }
    go (Fields fs _) = \b -> b{ fields = Just fs }
    -- Each accessory in the sequence overrides the next.
    -- TODO Could enforce only one accessory per section at the type level.
    go (EAppend x y) = go y . go x

context :: ContextBlock -> Blocks '[ContextBlock]
context c = Context c ()

context_ :: (Contains i ContextElementTypes) => Elements i -> Blocks '[ContextBlock]
context_ els = Context (go els defaultContext) ()
  where
    go :: ElementM i b -> ContextBlock -> ContextBlock
    go (TextObj t _) =
      \b -> b{ Context.elements = asContext t : Context.elements b }
    go (ImageE i _) =
      \b -> b{ Context.elements = asContext i : Context.elements b }
    go (EAppend x y) = go x . go y
    go (Fields _ _)  = id
    go (Button _ _)  = id

header :: HeaderBlock -> Blocks '[HeaderBlock]
header h = Header h ()

header_ :: Text -> Blocks '[HeaderBlock]
header_ txt = Header (defaultHeader txt) ()

actions :: ActionsBlock -> Blocks '[ActionsBlock]
actions a = Actions a ()

actions_ :: Contains i ActionsElementTypes => Elements i -> Blocks '[ActionsBlock]
actions_ els = Actions (go els defaultActions) ()
  where
    go :: ElementM i b -> ActionsBlock -> ActionsBlock
    go (Button i _)  = \b -> b{ Actions.elements = asAction i : Actions.elements b }
    go (EAppend x y) = go y . go x
    go (ImageE _ _)  = id
    go (Fields _ _)  = id
    go (TextObj _ _) = id

divider :: DividerBlock -> Blocks '[DividerBlock]
divider d = Divider d ()

divider_ :: Blocks '[DividerBlock]
divider_ = Divider defaultDivider ()
