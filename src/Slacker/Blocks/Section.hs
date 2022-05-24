{-# LANGUAGE OverloadedLabels #-}

module Slacker.Blocks.Section
  ( SectionBlock
  , section
  , section_
  , sectionNoText
  , sectionNoText_
  , Fields
  , field
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.DList.DNonEmpty as DL
import           Data.DList.DNonEmpty (DNonEmpty)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)
import           Named

import           Slacker.Blocks.Core
import           Slacker.Blocks.Elements

data SectionBlock
  = SectionBlock
  { sbContent   :: !SectionContent
  , sbBlockId   :: !(Maybe Text)
  , sbAccessory :: !(Maybe Elements)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON SectionBlock where
  toJSON SectionBlock{..} = Aeson.object $ catMaybes
    [ fmap ("block_id" Aeson..=) sbBlockId
    , fmap ("accessory" Aeson..=) sbAccessory
    , Just $ "type" Aeson..= Aeson.String "section"
    ] ++ contentFields
    where
      contentFields = case sbContent of
        TextOnly tObj ->
          ["text" Aeson..= tObj]
        TextAndFields tObj fs ->
          ["text" Aeson..= tObj, "fields" Aeson..= fs]
        FieldsOnly fs ->
          ["fields" Aeson..= fs]

data SectionContent
  = TextOnly TextObject
  | TextAndFields TextObject Fields
  | FieldsOnly Fields
    deriving stock (Generic, Show, Eq, Ord)

newtype Fields = Fields (DNonEmpty TextObject)
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON, Semigroup)

instance IsString Fields where
  fromString = Fields . pure . fromString

field :: TextObject -> Fields
field txt = Fields $ pure txt

section_ :: TextObject -> Blocks
section_ txt = section ! #text txt ! defaults

section
  :: "text" :! TextObject
  -> "fields" :? Fields
  -> "block_id" :? Text
  -> "accessory" :? Elements
  -> Blocks
section (Arg txt) (ArgF mFields) (ArgF mBlockId) (ArgF mAcc)
  = block
  $ SectionBlock
  { sbContent
  = case mFields of
      Just fs -> TextAndFields txt fs
      Nothing -> TextOnly txt
  , sbBlockId   = mBlockId
  , sbAccessory = element . DL.head . unElements <$> mAcc
  }

sectionNoText
  :: "fields" :! Fields
  -> "block_id" :? Text
  -> "accessory" :? Elements
  -> Blocks
sectionNoText (Arg fs) (ArgF mBlockId) (ArgF mAcc)
  = block
  $ SectionBlock
  { sbContent   = FieldsOnly fs
  , sbBlockId   = mBlockId
  , sbAccessory = mAcc
  }

sectionNoText_ :: Fields -> Blocks
sectionNoText_ fs = sectionNoText ! #fields fs ! defaults
