{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Slacker.Web.Files
  ( FilesUpload(..)
  , FileType(..)
  , File(..)
  , FileContent(..)
  , Filename
  , content
  , filepath
  , defaultFilesUpload
  , filesUpload
  , uploadJSON
  , uploadJSONText
  , uploadContent
  , uploadFile
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.IO.Unlift (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics (Generic)
import           Lens.Micro ((^.), (^?))
import           Lens.Micro.Aeson (_Bool, _String, _Value, key)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as HTTP
import qualified Network.HTTP.Simple as HTTP
import           Prelude hiding (id)
import qualified Prelude
import           UnliftIO.Exception (throwIO)

import           Slacker.Config (SlackConfig(..))
import qualified Slacker.Util as Util

data FilesUpload
  = FilesUpload
  { channels        :: !(Maybe Text)
  , file            :: !FileContent
  , filename        :: !(Maybe Text)
  , filetype        :: !(Maybe FileType)
  , initial_comment :: !(Maybe Text)
  , thread_ts       :: !(Maybe Text)
  , title           :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

data FileContent = FilePath Text | Content Text
  deriving (Eq, Show, Generic)

defaultFilesUpload :: FileContent -> FilesUpload
defaultFilesUpload fc
  = FilesUpload
  { channels        = Nothing
  , file            = fc
  , filename        = Nothing
  , filetype        = Nothing
  , initial_comment = Nothing
  , thread_ts       = Nothing
  , title           = Nothing
  }

content :: Text -> FileContent
content = Content

filepath :: Text -> FileContent
filepath = FilePath

type Filename = Text

-- | Short-cut to upload a ToJSON-able type as a file.
uploadJSON
  :: (Aeson.ToJSON a, MonadIO m)
  => SlackConfig
  -> Filename
  -> a
  -> m File
uploadJSON cfg name json = filesUpload (slackApiToken cfg) fup
  where
    newName
      | ".json" `T.isSuffixOf` name = name
      | otherwise = name <> ".json"
    fup = (defaultFilesUpload (content $ Util.toJSONText json))
        { filename = Just newName
        }

-- | Short-cut to upload JSON text as a file.
uploadJSONText
  :: (MonadIO m)
  => SlackConfig
  -> Filename
  -> Text
  -> m File
uploadJSONText cfg name json = filesUpload (slackApiToken cfg) fup
  where
    newName
      | ".json" `T.isSuffixOf` name = name
      | otherwise = name <> ".json"
    fup = (defaultFilesUpload (content json))
        { filename = Just newName
        }

-- | Upload a file from a local filepath.
uploadFile
  :: (MonadIO m)
  => SlackConfig
  -> Text
  -> m File
uploadFile cfg fp = filesUpload (slackApiToken cfg) fup
  where
    fup = defaultFilesUpload (filepath fp)

-- | Upload text content from memory.
uploadContent
  :: (MonadIO m)
  => SlackConfig
  -> Filename
  -> Text
  -> m File
uploadContent cfg name txt = filesUpload (slackApiToken cfg) fup
  where
    fup = (defaultFilesUpload (content txt))
        { filename = Just name
        }

-- | Make a files.upload request to the Slack Web API. Requires files:write scope.
filesUpload
  :: MonadIO m
  => Text
  -> FilesUpload
  -> m File
filesUpload auth fup = do
  req
    <- liftIO
     $ HTTP.parseRequest "https://slack.com/api/files.upload"
     >>= HTTP.formDataBody (toFormData fup)
  resp
    <- fmap HTTP.responseBody
     . HTTP.httpJSON
     . HTTP.addRequestHeader "Authorization" ("Bearer " <> T.encodeUtf8 auth)
     $ req
  case ((resp :: Aeson.Value) ^? key "ok"  . _Bool, resp ^? key "file" . _Value) of
    (Just True, Just val) -> either (throwIO . userError) pure (Aeson.parseEither Aeson.parseJSON val)
    (Just False, _)       -> throwIO $ userError $ T.unpack $ resp ^. key "error" . _String
    (Just True, Nothing)  -> throwIO $ userError "Couldn't parse key 'file from response"
    (Nothing, _)          -> throwIO $ userError "Couldn't parse key 'ok' from response"

toFormData :: FilesUpload -> [HTTP.Part]
toFormData FilesUpload{..} = catMaybes
  [ textPart "channels" <$> channels
  , textPart "filename" <$> filename
  , textPart "filetype" . filetypeToText <$> filetype
  , textPart "initial_comment" <$> initial_comment
  , textPart "thread_ts" <$> thread_ts
  , textPart "title" <$> title
  , Just . fileToFormData $ file
  ]

fileToFormData :: FileContent -> HTTP.Part
fileToFormData = \case
  FilePath fp -> HTTP.partFileSource "file" $ T.unpack fp
  Content txt -> HTTP.partBS "content" $ T.encodeUtf8 txt

textPart :: Text -> Text -> HTTP.Part
textPart name = HTTP.partBS name . T.encodeUtf8

data FileType
  = Auto -- Auto Detect Type
  | Text -- Plain Text
  | Ai -- Illustrator File
  | Apk -- APK
  | Applescript -- AppleScript
  | Binary -- Binary
  | Bmp -- Bitmap
  | Boxnote -- BoxNote
  | C -- C
  | Csharp -- C#
  | Cpp -- C++
  | Css -- CSS
  | Csv -- CSV
  | Clojure -- Clojure
  | Coffeescript -- CoffeeScript
  | Cfm -- ColdFusion
  | D -- D
  | Dart -- Dart
  | Diff -- Diff
  | Doc -- Word Document
  | Docx -- Word document
  | Dockerfile -- Docker
  | Dotx -- Word template
  | Email -- Email
  | Eps -- EPS
  | Epub -- EPUB
  | Erlang -- Erlang
  | Fla -- Flash FLA
  | Flv -- Flash video
  | Fsharp -- F#
  | Fortran -- Fortran
  | Gdoc -- GDocs Document
  | Gdraw -- GDocs Drawing
  | Gif -- GIF
  | Go -- Go
  | Gpres -- GDocs Presentation
  | Groovy -- Groovy
  | Gsheet -- GDocs Spreadsheet
  | Gzip -- Gzip
  | Html -- HTML
  | Handlebars -- Handlebars
  | Haskell -- Haskell
  | Haxe -- Haxe
  | Indd -- InDesign Document
  | Java -- Java
  | Javascript -- JavaScript/JSON
  | Jpg -- JPEG
  | Keynote -- Keynote Document
  | Kotlin -- Kotlin
  | Latex -- LaTeX/sTeX
  | Lisp -- Lisp
  | Lua -- Lua
  | M4a -- MPEG 4 audio
  | Markdown -- Markdown (raw)
  | Matlab -- MATLAB
  | Mhtml -- MHTML
  | Mkv -- Matroska video
  | Mov -- QuickTime video
  | Mp3 -- mp4
  | Mp4 -- MPEG 4 video
  | Mpg -- MPEG video
  | Mumps -- MUMPS
  | Numbers -- Numbers Document
  | Nzb -- NZB
  | Objc -- Objective-C
  | Ocaml -- OCaml
  | Odg -- OpenDocument Drawing
  | Odi -- OpenDocument Image
  | Odp -- OpenDocument Presentation
  | Ods -- OpenDocument Spreadsheet
  | Odt -- OpenDocument Text
  | Ogg -- Ogg Vorbis
  | Ogv -- Ogg video
  | Pages -- Pages Document
  | Pascal -- Pascal
  | Pdf -- PDF
  | Perl -- Perl
  | Php -- PHP
  | Pig -- Pig
  | Png -- PNG
  | Post -- Slack Post
  | Powershell -- PowerShell
  | Ppt -- PowerPoint presentation
  | Pptx -- PowerPoint presentation
  | Psd -- Photoshop Document
  | Puppet -- Puppet
  | Python -- Python
  | Qtz -- Quartz Composer Composition
  | R -- R
  | Rtf -- Rich Text File
  | Ruby -- Ruby
  | Rust -- Rust
  | Sql -- SQL
  | Sass -- Sass
  | Scala -- Scala
  | Scheme -- Scheme
  | Sketch -- Sketch File
  | Shell -- Shell
  | Smalltalk -- Smalltalk
  | Svg -- SVG
  | Swf -- Flash SWF
  | Swift -- Swift
  | Tar -- Tarball
  | Tiff -- TIFF
  | Tsv -- TSV
  | Vb -- VB.NET
  | Vbscript -- VBScript
  | Vcard -- vCard
  | Velocity -- Velocity
  | Verilog -- Verilog
  | Wav -- Waveform audio
  | Webm -- WebM
  | Wmv -- Windows Media Video
  | Xls -- Excel spreadsheet
  | Xlsx -- Excel spreadsheet
  | Xlsb -- Excel Spreadsheet (Binary, Macro Enabled)
  | Xlsm -- Excel Spreadsheet (Macro Enabled)
  | Xltx -- Excel template
  | Xml -- XML
  | Yaml -- YAML
  | Zip -- Zip
  deriving (Eq, Ord, Bounded, Enum, Generic, Show)

filetypeToText :: FileType -> Text
filetypeToText = T.toLower . T.pack . show

filetypeFromText :: Text -> Maybe FileType
filetypeFromText ft = lookup ft $ (filetypeToText &&& Prelude.id) <$> [minBound..]

instance Aeson.FromJSON FileType where
  parseJSON = Aeson.withText "filetype" $ \ft ->
    maybe (fail "unknown filetype") pure (filetypeFromText ft)

data File
  = File
  { fileChannels           :: ![Text]
  , fileCommentsCount      :: !(Maybe Int)
  , fileCreated            :: !Int
  , fileDeanimateGif       :: !(Maybe Text)
  , fileDisplayAsBot       :: !Bool
  , fileEditable           :: !Bool
  , fileEditLink           :: !(Maybe Text)
  , fileExternalType       :: !Text
  , fileFiletype           :: !FileType
  , fileGroups             :: ![Text]
  , fileHasRichPreview     :: !Bool
  , fileId                 :: !Text
  , fileImageExifRotation  :: !(Maybe Int)
  , fileInitialComment     :: !(Maybe Text)
  , fileIms                :: ![Text]
  , fileIsExternal         :: !Bool
  , fileIsPublic           :: !Bool
  , fileIsStarred          :: !(Maybe Bool)
  , fileMimetype           :: !Text
  , fileMode               :: !Text
  , fileName               :: !(Maybe Text)
  , fileNumStars           :: !(Maybe Int)
  , fileOriginalH          :: !(Maybe Int)
  , fileOriginalW          :: !(Maybe Int)
  , filePermalink          :: !Text
  , filePermalinkPublic    :: !(Maybe Text)
  , filePjpeg              :: !(Maybe Text)
  , filePrettyType         :: !Text
  , filePublicUrlShared    :: !Bool
  , fileSize               :: !Int
  , fileThumb160           :: !(Maybe Text)
  , fileThumb360           :: !(Maybe Text)
  , fileThumb360Gif        :: !(Maybe Text)
  , fileThumb360H          :: !(Maybe Int)
  , fileThumb360W          :: !(Maybe Int)
  , fileThumb64            :: !(Maybe Text)
  , fileThumb80            :: !(Maybe Text)
  , fileTitle              :: !Text
  , fileUrlPrivate         :: !(Maybe Text)
  , fileUrlPrivateDownload :: !(Maybe Text)
  , fileUser               :: !(Maybe Text)
  , fileUsername           :: !Text
  } deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON File where
  parseJSON
    = Aeson.genericParseJSON
    $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 4
    }

