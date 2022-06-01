module Slacker.Blocks.Elements.TextObject
  ( TextObject(..)
  , MarkdownTextObject(..)
  , PlainTextObject(..)
  , HasText(..)
  , markdownObj
  , plaintextObj
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

class HasText a where
  markdown :: Text -> a
  plaintext :: Text -> a

instance HasText TextObject where
  markdown txt  = markdownObj txt
  plaintext txt = plaintextObj txt

instance Aeson.ToJSON TextObject where
  toJSON (PlainText ptxt) = Aeson.toJSON ptxt
  toJSON (MarkdownText mtxt) = Aeson.toJSON mtxt

instance IsString TextObject where
  fromString = markdownObj . fromString

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

markdownObj :: Text -> TextObject
markdownObj = MarkdownText . MarkdownTextObject

plaintextObj :: Text -> TextObject
plaintextObj = PlainText . PlainTextObject

plaintext_ :: Text -> PlainTextObject
plaintext_ = PlainTextObject

embolden :: Text -> TextObject
embolden txt = markdownObj $ "*" <> txt <> "*"

italicize :: Text -> TextObject
italicize txt = markdownObj $ "_" <> txt <> "_"
