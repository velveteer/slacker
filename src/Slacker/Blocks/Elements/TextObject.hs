module Slacker.Blocks.Elements.TextObject
  ( TextObject
  , MarkdownTextObject(..)
  , PlainTextObject(..)
  , markdown
  , plaintext
  , plaintext_
  , embolden
  , italicize
  ) where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)

data TextObject
  = PlainText !PlainTextObject
  | MarkdownText !MarkdownTextObject
  deriving stock (Generic, Show, Eq, Ord)

instance Aeson.ToJSON TextObject where
  toJSON (PlainText ptxt) = Aeson.toJSON ptxt
  toJSON (MarkdownText mtxt) = Aeson.toJSON mtxt

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

newtype MarkdownTextObject = MarkdownTextObject Text
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (IsString, Semigroup)

instance Aeson.ToJSON MarkdownTextObject where
  toJSON (MarkdownTextObject txt)
    = Aeson.object
    [ "type" Aeson..= Aeson.String "mrkdwn"
    , "text" Aeson..= txt
    ]

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
