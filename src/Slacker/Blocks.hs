{-# LANGUAGE CPP #-}
module Slacker.Blocks
  ( Block(..)
  , ImageBlock(..)
  , TextObject(..)
  , embolden
  , image
  , imageNoTitle
  , markdown
  , markdownSection
  , plaintext
  ) where

import qualified Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
#else
import qualified Data.HashMap.Strict as HM
#endif
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)

-- TODO Lots of block types to model here.
data Block
  = Image ImageBlock
  | Section SectionBlock
  deriving stock (Generic, Show, Eq, Ord)

toJSONWithTypeField :: Text -> Aeson.Value -> Aeson.Value
toJSONWithTypeField typ (Aeson.Object obj) =
#if MIN_VERSION_aeson(2,0,0)
  Aeson.Object $ Aeson.insert (Aeson.fromText "type") (Aeson.String typ) obj
#else
  Aeson.Object $ HM.insert "type" (Aeson.String typ) obj
#endif
toJSONWithTypeField _ val = val

instance Aeson.ToJSON Block where
  toJSON (Image b)   = toJSONWithTypeField "image" (Aeson.toJSON b)
  toJSON (Section b) = toJSONWithTypeField "section" (Aeson.toJSON b)

instance Aeson.FromJSON Block where
  parseJSON val = flip (Aeson.withObject "Block") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "image"   -> Image <$> Aeson.parseJSON val
      "section" -> Section <$> Aeson.parseJSON val
      unknown   -> fail $ "Unknown block type: " <> T.unpack unknown

data ImageBlock
  = ImageBlock
  { ibTitle    :: !(Maybe TextObject)
  , ibBlockId  :: !(Maybe Text)
  , ibImageUrl :: !Text
  , ibAltText  :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

image :: TextObject -> Text -> Text -> Block
image title url alt
  = Image $ ImageBlock
  { ibTitle    = Just title
  , ibBlockId  = Nothing
  , ibImageUrl = url
  , ibAltText  = alt
  }

imageNoTitle :: Text -> Text -> Block
imageNoTitle url alt
  = Image $ ImageBlock
  { ibTitle    = Nothing
  , ibBlockId  = Nothing
  , ibImageUrl = url
  , ibAltText  = alt
  }

instance Aeson.ToJSON ImageBlock where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

instance Aeson.FromJSON ImageBlock where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data SectionBlock
  = SectionBlock
  { sbText    :: !TextObject
  , sbBlockId :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

markdownSection :: Text -> Block
markdownSection txt = Section $ SectionBlock (markdown txt) Nothing

instance Aeson.ToJSON SectionBlock where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

instance Aeson.FromJSON SectionBlock where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

data TextObject
  = PlainText !Text
  | MarkdownText !Text
  deriving stock (Generic, Show, Eq, Ord)

markdown :: Text -> TextObject
markdown = MarkdownText

embolden :: Text -> Text
embolden txt = "*" <> txt <> "*"

plaintext :: Text -> TextObject
plaintext = PlainText

instance Aeson.ToJSON TextObject where
  toJSON (PlainText txt)
    = Aeson.object
    [ "type" Aeson..= Aeson.String "plain_text"
    , "text" Aeson..= txt
    ]
  toJSON (MarkdownText txt)
    = Aeson.object
    [ "type" Aeson..= Aeson.String "mrkdwn"
    , "text" Aeson..= txt
    ]

instance Aeson.FromJSON TextObject where
  parseJSON val = flip (Aeson.withObject "TextObject") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "plain_text" -> PlainText <$> Aeson.parseJSON val
      "mrkdwn"     -> MarkdownText <$> Aeson.parseJSON val
      unknown      -> fail $ "Unknown text type: " <> T.unpack unknown

