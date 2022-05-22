{-# LANGUAGE CPP #-}
module Slacker.Util
  ( assertJust
  , toJSONText
  , toJSONWithTypeField
  ) where

import           Control.Monad.IO.Unlift (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
#else
import qualified Data.HashMap.Strict as HM
#endif
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           UnliftIO.Exception

toJSONWithTypeField :: Text -> Aeson.Value -> Aeson.Value
toJSONWithTypeField typ (Aeson.Object obj) =
#if MIN_VERSION_aeson(2,0,0)
  Aeson.Object $ Aeson.insert (Aeson.fromText "type") (Aeson.String typ) obj
#else
  Aeson.Object $ HM.insert "type" (Aeson.String typ) obj
#endif
toJSONWithTypeField _ val = val

toJSONText :: Aeson.ToJSON a => a -> Text
toJSONText = TL.toStrict . Aeson.encodeToLazyText

assertJust :: MonadIO m => Maybe a -> Text -> m a
assertJust mx e = maybe (throwIO $ userError $ T.unpack e) pure mx
