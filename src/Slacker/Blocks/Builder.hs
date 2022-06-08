{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-redundant-constraints #-}

module Slacker.Blocks.Builder
  ( Blocks(..)
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
import           Slacker.Blocks.Divider
import           Slacker.Blocks.Elements
import           Slacker.Blocks.Header
import           Slacker.Blocks.Image
import           Slacker.Blocks.Section
import qualified Slacker.Blocks.Section as Section

data Blocks i where
  Section :: SectionBlock -> Blocks '[SectionBlock]
  Header  :: HeaderBlock -> Blocks '[HeaderBlock]
  Context :: ContextBlock -> Blocks '[ContextBlock]
  Divider :: DividerBlock -> Blocks '[DividerBlock]
  Image   :: ImageBlock -> Blocks '[ImageBlock]
  Actions :: ActionsBlock -> Blocks '[ActionsBlock]
  Append  :: Blocks as -> Blocks bs -> Blocks (as ++ bs)

instance IxAppend Blocks where
  (>>) = Append

instance (i ~ '[Type]) => Aeson.ToJSON (Blocks i) where
  toJSON bs = Aeson.toJSON $ blocksToValues bs []

instance (i ~ '[ImageBlock]) => HasImage ImageBlock (Blocks i) where
  image el = Image el
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
    go :: Blocks i -> [OpenUnion AllBlocks] -> [OpenUnion AllBlocks]
    go (Section b)  = (openUnionLift b :)
    go (Header b)   = (openUnionLift b :)
    go (Context b)  = (openUnionLift b :)
    go (Actions b)  = (openUnionLift b :)
    go (Divider b)  = (openUnionLift b :)
    go (Image b)    = (openUnionLift b :)
    go (Append x y) = go x . go y

blocksToValues :: Blocks i -> [Aeson.Value] -> [Aeson.Value]
blocksToValues = go
  where
    go :: Blocks i -> [Aeson.Value] -> [Aeson.Value]
    go (Section b)  = (Aeson.toJSON b :)
    go (Header b)   = (Aeson.toJSON b :)
    go (Context b)  = (Aeson.toJSON b :)
    go (Actions b)  = (Aeson.toJSON b :)
    go (Divider b)  = (Aeson.toJSON b :)
    go (Image b)    = (Aeson.toJSON b :)
    go (Append x y) = go x . go y

section :: SectionBlock -> Blocks '[SectionBlock]
section = Section

section_
  :: (Contains i (TextObject ': SectionFields ': SectionAccessoryTypes))
  => Elements i
  -> Blocks '[SectionBlock]
section_ els = Section $ go els (defaultSection "")
  where
    go :: Elements i -> SectionBlock -> SectionBlock
    go (TextObj t) = \b -> b{ Section.text = t }
    go (Button bb) = \b -> b{ accessory = Just $ asAccessory bb }
    go (ImageE i)  = \b -> b{ accessory = Just $ asAccessory i }
    go (Fields fs) = \b -> b{ fields = Just fs }
    -- Each accessory in the sequence overrides the next.
    -- TODO Could enforce only one accessory per section at the type level.
    go (EAppend x y) = go y . go x

context :: ContextBlock -> Blocks '[ContextBlock]
context = Context

context_ :: (Contains i ContextElementTypes) => Elements i -> Blocks '[ContextBlock]
context_ = Context . defaultContext . go
  where
    go :: Elements i -> ContextElements
    go (TextObj t)   = asContext t
    go (ImageE i )   = asContext i
    go (EAppend x y) = go x <> go y
    go _             = error "impossible context element"

header :: HeaderBlock -> Blocks '[HeaderBlock]
header = Header

header_ :: Text -> Blocks '[HeaderBlock]
header_ txt = Header (defaultHeader txt)

actions :: ActionsBlock -> Blocks '[ActionsBlock]
actions = Actions

actions_ :: Contains i ActionsElementTypes => Elements i -> Blocks '[ActionsBlock]
actions_ = Actions . defaultActions . go
  where
    go :: Elements i -> ActionsBlock -> ActionsBlock
    go (Button b)    = asAction b
    go (EAppend x y) = go x <> go y
    go _             = error "impossible action element"

divider :: DividerBlock -> Blocks '[DividerBlock]
divider = Divider

divider_ :: Blocks '[DividerBlock]
divider_ = Divider defaultDivider
