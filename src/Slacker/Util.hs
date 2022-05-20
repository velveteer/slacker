module Slacker.Util
  ( assertJust
  , toJSONText
  ) where

import           Control.Monad.IO.Unlift (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           UnliftIO.Exception

toJSONText :: Aeson.ToJSON a => a -> Text
toJSONText = TL.toStrict . Aeson.encodeToLazyText

assertJust :: MonadIO m => Maybe a -> Text -> m a
assertJust mx e = maybe (throwIO $ userError $ T.unpack e) pure mx
