module Slacker.Util
  ( assertJust
  , toJSONText
  , toJSONWithTypeField
  , genericFromJSONWithType
  ) where

import           Control.Monad.IO.Unlift (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           GHC.Generics (Generic, Rep)
import           UnliftIO.Exception

toJSONWithTypeField :: Text -> Aeson.Value -> Aeson.Value
toJSONWithTypeField typ (Aeson.Object obj) =
  Aeson.Object $ Aeson.insert (Aeson.fromText "type") (Aeson.String typ) obj
toJSONWithTypeField _ val = val

genericFromJSONWithType
  :: (Aeson.GFromJSON Aeson.Zero (Rep a), Generic a)
  => String
  -> Text
  -> Aeson.Value -> Aeson.Parser a
genericFromJSONWithType lbl t val =
  flip (Aeson.withObject lbl) val $ \o -> do
    typ <- o Aeson..: "type"
    flip (Aeson.withText "type") typ $ \case
      evt | evt == t -> Aeson.genericParseJSON Aeson.defaultOptions val
      evt -> fail $ "expected type of " <> T.unpack t <> " but was given " <> T.unpack evt

toJSONText :: Aeson.ToJSON a => a -> Text
toJSONText = TL.toStrict . Aeson.encodeToLazyText

assertJust :: MonadIO m => Maybe a -> Text -> m a
assertJust mx e = maybe (throwIO $ userError $ T.unpack e) pure mx
