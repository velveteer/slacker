module Slacker.Blocks
  ( -- * Layout blocks
    Block(..)
  , Blocks
  , HeaderBlock(..)
  , header
  , DividerBlock(..)
  , divider
  , SectionBlock(..)
  , sectionText_
  , sectionText
  , sectionFields_
  , sectionFields
  , sectionTextFields_
  , sectionTextFields
  , field
  , ImageBlock(..)
  , image
  , image_
  , TextObject(..)
  , markdown
  , plaintext
  , embolden
  , italicize
    -- * Elements
  , InteractiveElement(..)
  , ButtonElement(..)
  , button
  ) where

import qualified Data.Aeson as Aeson
import           Data.DList.DNonEmpty (DNonEmpty)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)

import Slacker.Util (toJSONWithTypeField)

type Blocks = DNonEmpty Block

data Block
  = Image ImageBlock
  | Section SectionBlock
  | Divider DividerBlock
  | Header HeaderBlock
  deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON Block where
  toJSON (Image b)   = toJSONWithTypeField "image" (Aeson.toJSON b)
  toJSON (Section b) = toJSONWithTypeField "section" (Aeson.toJSON b)
  toJSON (Header b)  = toJSONWithTypeField "header" (Aeson.toJSON b)
  toJSON (Divider b) = toJSONWithTypeField "divider" (Aeson.toJSON b)

instance Aeson.FromJSON Block where
  parseJSON val = flip (Aeson.withObject "Block") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "image"   -> Image <$> Aeson.parseJSON val
      "section" -> Section <$> Aeson.parseJSON val
      "header"  -> Header <$> Aeson.parseJSON val
      "divider" -> Divider <$> Aeson.parseJSON val
      unknown   -> fail $ "Unknown block type: " <> T.unpack unknown

data DividerBlock
  = DividerBlock
  { dbBlockId :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON DividerBlock where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

instance Aeson.FromJSON DividerBlock where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

divider :: Blocks
divider = pure . Divider $ DividerBlock Nothing

data HeaderBlock
  = HeaderBlock
  { hbBlockId :: !(Maybe Text)
  , hbText    :: !PlainTextObject
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON HeaderBlock where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    , Aeson.omitNothingFields = True
    }

instance Aeson.FromJSON HeaderBlock where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
    }

header :: Text -> Blocks
header txt = pure . Header $ HeaderBlock
  { hbBlockId = Nothing
  , hbText    = plaintext_ txt
  }

data ImageBlock
  = ImageBlock
  { ibTitle    :: !(Maybe PlainTextObject)
  , ibBlockId  :: !(Maybe Text)
  , ibImageUrl :: !Text
  , ibAltText  :: !Text
  } deriving stock (Generic, Show, Eq, Ord)

-- | Create image block with a plaintext title.
image :: Text -> Text -> Text -> Blocks
image title url alt
  = pure . Image $ ImageBlock
  { ibTitle    = Just $ plaintext_ title
  , ibBlockId  = Nothing
  , ibImageUrl = url
  , ibAltText  = alt
  }

-- | Create image block without a title.
image_ :: Text -> Text -> Blocks
image_ url alt
  = pure . Image $ ImageBlock
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
  { sbContent   :: !SectionContent
  , sbBlockId   :: !(Maybe Text)
  , sbAccessory :: !(Maybe InteractiveElement)
  } deriving stock (Generic, Show, Eq, Ord)

data SectionContent
  = TextOnly TextObject
  | TextAndFields TextObject Fields
  | FieldsOnly Fields
    deriving stock (Generic, Show, Eq, Ord)

newtype Fields = Fields { unFields :: DNonEmpty TextObject }
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON, Semigroup)

instance IsString Fields where
  fromString = Fields . pure . fromString

field :: TextObject -> Fields
field txt = Fields $ pure txt

sectionText_ :: TextObject -> Blocks
sectionText_ txt = pure . Section $ SectionBlock
  { sbContent   = TextOnly txt
  , sbBlockId   = Nothing
  , sbAccessory = Nothing
  }

sectionText :: TextObject -> InteractiveElement -> Blocks
sectionText txt acc = pure . Section $ SectionBlock
  { sbContent   = TextOnly txt
  , sbBlockId   = Nothing
  , sbAccessory = Just acc
  }

sectionFields_ :: Fields -> Blocks
sectionFields_ fs = pure . Section $ SectionBlock
  { sbContent   = FieldsOnly fs
  , sbBlockId   = Nothing
  , sbAccessory = Nothing
  }

sectionFields :: Fields -> InteractiveElement -> Blocks
sectionFields fs acc = pure . Section $ SectionBlock
  { sbContent   = FieldsOnly fs
  , sbBlockId   = Nothing
  , sbAccessory = Just acc
  }

sectionTextFields_ :: TextObject -> Fields -> Blocks
sectionTextFields_ txt fs = pure . Section $ SectionBlock
  { sbContent   = TextAndFields txt fs
  , sbBlockId   = Nothing
  , sbAccessory = Nothing
  }

sectionTextFields :: TextObject -> Fields -> InteractiveElement -> Blocks
sectionTextFields txt fs acc = pure . Section $ SectionBlock
  { sbContent   = TextAndFields txt fs
  , sbBlockId   = Nothing
  , sbAccessory = Just acc
  }

instance Aeson.ToJSON SectionBlock where
  toJSON SectionBlock{..}
    = Aeson.object $ catMaybes
    [ fmap ("block_id" Aeson..=) sbBlockId
    , fmap ("accessory" Aeson..=) sbAccessory
    ] ++ contentFields
    where
      contentFields = case sbContent of
        TextOnly tObj ->
          ["text" Aeson..= tObj]
        TextAndFields tObj fs ->
          ["text" Aeson..= tObj, "fields" Aeson..= fs]
        FieldsOnly fs ->
          ["fields" Aeson..= fs]

instance Aeson.FromJSON SectionBlock where
  parseJSON val = flip (Aeson.withObject "SectionBlock") val $ \obj -> do
    mTxt     <- obj Aeson..:? "text"
    mFields  <- obj Aeson..:? "fields"
    mBlockId <- obj Aeson..:? "block_id"
    mAcc     <- obj Aeson..:? "accessory"
    content  <-
      case (mTxt, mFields) of
        (Just txt, Nothing) ->
          TextOnly <$> Aeson.parseJSON txt
        (Just txt, Just fs) ->
          TextAndFields <$> Aeson.parseJSON txt <*> Aeson.parseJSON fs
        (Nothing, Just fs) ->
          FieldsOnly <$> Aeson.parseJSON fs
        (Nothing, Nothing) ->
          fail "Section does not contain any text content"
    pure $ SectionBlock content mBlockId mAcc

-- ==================================================
-- | Text Objects
-- ==================================================

data TextObject
  = PlainText !PlainTextObject
  | MarkdownText !MarkdownTextObject
  deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON TextObject where
  toJSON (PlainText ptxt) = Aeson.toJSON ptxt
  toJSON (MarkdownText mtxt) = Aeson.toJSON mtxt

instance Aeson.FromJSON TextObject where
  parseJSON val = flip (Aeson.withObject "TextObject") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "plain_text" -> PlainText <$> Aeson.parseJSON val
      "mrkdwn"     -> MarkdownText <$> Aeson.parseJSON val
      unknown      -> fail $ "Unknown text type: " <> T.unpack unknown

instance IsString TextObject where
  fromString = markdown . fromString

instance Semigroup TextObject where
  PlainText a <> PlainText b = PlainText $ a <> b
  MarkdownText a <> MarkdownText b = MarkdownText $ a <> b
  PlainText (PlainTextObject a) <> MarkdownText b = MarkdownText $ MarkdownTextObject a <> b
  MarkdownText a <> PlainText (PlainTextObject b) = MarkdownText $ a <> MarkdownTextObject b

newtype PlainTextObject = PlainTextObject Text
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (IsString, Semigroup)

instance Aeson.ToJSON PlainTextObject where
  toJSON (PlainTextObject txt)
    = Aeson.object
    [ "type" Aeson..= Aeson.String "plain_text"
    , "text" Aeson..= txt
    ]

instance Aeson.FromJSON PlainTextObject where
  parseJSON val = flip (Aeson.withObject "PlainTextObject") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "plain_text" -> PlainTextObject <$> Aeson.parseJSON val
      unknown      -> fail $ "Expected plain_text type but got: " <> T.unpack unknown

newtype MarkdownTextObject = MarkdownTextObject Text
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (IsString, Semigroup)

instance Aeson.ToJSON MarkdownTextObject where
  toJSON (MarkdownTextObject txt)
    = Aeson.object
    [ "type" Aeson..= Aeson.String "mrkdwn"
    , "text" Aeson..= txt
    ]

instance Aeson.FromJSON MarkdownTextObject where
  parseJSON val = flip (Aeson.withObject "MarkdownTextObject") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "mrkdwn" -> MarkdownTextObject <$> Aeson.parseJSON val
      unknown  -> fail $ "Expected mrkdwn type but got: " <> T.unpack unknown

markdown :: Text -> TextObject
markdown = MarkdownText . MarkdownTextObject

plaintext :: Text -> TextObject
plaintext = PlainText . PlainTextObject

plaintext_ :: Text -> PlainTextObject
plaintext_ = PlainTextObject

embolden :: Text -> TextObject
embolden txt = markdown $ "*" <> txt <> "*"

italicize :: Text -> TextObject
italicize txt = markdown $ "_" <> txt <> "_"

-- ==================================================
-- | Elements
-- ==================================================

data InteractiveElement
  = Button ButtonElement
  deriving (Generic, Eq, Ord, Show)

instance Aeson.ToJSON InteractiveElement where
  toJSON (Button b) = toJSONWithTypeField "button" (Aeson.toJSON b)

instance Aeson.FromJSON InteractiveElement where
  parseJSON val = flip (Aeson.withObject "Element") val $ \obj -> do
    typ <- obj Aeson..: "type"
    case typ of
      "button" -> Button <$> Aeson.parseJSON val
      unknown  -> fail $ "Unknown element type: " <> T.unpack unknown

data ButtonElement
  = ButtonElement
  { bText               :: !PlainTextObject
  , bActionId           :: !Text
  , bUrl                :: !(Maybe Text)
  , bValue              :: !(Maybe Text)
  , bStyle              :: !(Maybe Text)
  , bAccessibilityLabel :: !(Maybe Text)
  } deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON ButtonElement where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 1
    , Aeson.omitNothingFields = True
    }

instance Aeson.FromJSON ButtonElement where
  parseJSON
    = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 1
    }

button :: Text -> Text -> InteractiveElement
button txt actId = Button $ ButtonElement
  { bText               = plaintext_ txt
  , bActionId           = actId
  , bUrl                = Nothing
  , bValue              = Nothing
  , bStyle              = Nothing
  , bAccessibilityLabel = Nothing
  }

