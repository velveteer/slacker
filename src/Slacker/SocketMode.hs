module Slacker.SocketMode
  ( module Export
  , SocketModeEnv(slackConfig)
  , ThreadError(..)
  , getNextEvent
  , handleEvents
  , handleThreadExceptionSensibly
  , initSocketMode
  , runSocketMode
  , shutdownSocketMode
  ) where

import           Control.Concurrent.STM.TBMQueue
import           Control.Lens ((^?))
import           Control.Monad (void)
import           Control.Monad.IO.Unlift (MonadIO, liftIO)
import           Control.Monad.Logger.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens
import           Data.Foldable (for_, traverse_)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           GHC.IO.Exception
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import           UnliftIO.Async
  ( Async
  , AsyncCancelled(..)
  , asyncWithUnmask
  , uninterruptibleCancel
  , waitAnyCatch
  , waitCatch
  )
import           UnliftIO.Exception
  ( Exception
  , SomeException
  , catch
  , fromException
  , mask_
  , throwIO
  , uninterruptibleMask_
  )
import           UnliftIO.STM
import qualified Wuss as WS

import           Slacker.Config (SlackConfig(..), withLogLevel)
import           Slacker.SocketMode.Types as Export
import           Slacker.Util (assertJust, toJSONText)
import           Slacker.Web (makeSlackPostJSONNoBody)

data SocketModeEnv
  = SocketModeEnv
  { slackConfig   :: !SlackConfig
  , inboundQueue  :: !(TBMQueue SocketModeEvent)
  , bgThreads     :: !(TVar [(Async (), Int)])
  , refreshLock   :: !(TMVar ())
  , shutdownVar   :: !(TMVar ())
  , startVar      :: !(TMVar ())
  } deriving stock (Generic)

runSocketMode :: MonadIO m => SlackConfig -> (SlackConfig -> SocketModeEvent -> m ()) -> m ()
runSocketMode cfg h = liftIO (initSocketMode cfg) >>= flip handleEvents h

initSocketMode :: SlackConfig -> IO SocketModeEnv
initSocketMode cfg = mask_ . runStdoutLoggingT . withLogLevel (logLevel cfg) $ do
  logDebug "setting up"
  iqueue      <- liftIO $ newTBMQueueIO (inboundQueueMax cfg)
  tsTVar      <- liftIO $ newTVarIO []
  refLock     <- liftIO newEmptyTMVarIO
  shutdownVar <- liftIO newEmptyTMVarIO
  startVar    <- liftIO newEmptyTMVarIO
  let env = SocketModeEnv cfg iqueue tsTVar refLock shutdownVar startVar
  for_ [1..numThreads cfg] $ \tId -> do
    t <- liftIO $ asyncWithUnmask (\um -> um $ pollSocket env tId)
    atomically $ modifyTVar' tsTVar ((t, tId) :)
  void . liftIO $ spawnThreadMonitor env
  void . liftIO $ spawnShutdownHandler env
  liftIO $ gracefulShutdownHandler cfg (shutdownSocketMode env)
  logDebug "done setting up"
  liftIO . atomically $ putTMVar startVar ()
  logInfo "socket mode starting"
  pure env

data ThreadError
  = ConnectionError SomeException
  | JSONDecodeError String
  deriving (Show, Generic)

instance Exception ThreadError

-- | Spawns a thread to restart any threads based on the result of the exception handler.
spawnThreadMonitor :: SocketModeEnv -> IO (Async ())
spawnThreadMonitor env = asyncWithUnmask $ \unmask -> unmask loop
  where
    cfg = slackConfig env
    loop = do
      threads <- readTVarIO $ bgThreads env
      (stoppedAsync, x) <- waitAnyCatch (fst <$> threads)
      case x of
        Left err | Just AsyncCancelled <- fromException err -> pure ()
        Left err | Just tId <- lookup stoppedAsync threads -> do
          shouldRestart <- onException cfg cfg err tId
          if shouldRestart
            then restartFailed env threads tId >> loop
            else shutdownSocketMode env
        _ -> do
          -- Polling thread exited normally, which shouldn't ever happen.
          runStdoutLoggingT
            . withLogLevel (logLevel cfg)
            $ logError "a thread exited without exception, we're shutting it all down"
          shutdownSocketMode env

restartFailed :: SocketModeEnv -> [(Async (), Int)] -> Int -> IO ()
restartFailed env threads tId = mask_ $ do
  t <- asyncWithUnmask (\um -> um $ pollSocket env tId)
  let newThreads = (t, tId) : filter ((/= tId) . snd) threads
  void . atomically $ swapTVar (bgThreads env) newThreads

-- | If an ephemeral error (like decoding JSON) or unexpected disconnect occurs
-- then restart the connection. This is not the default handler but is included
-- as an opinionated helper.
handleThreadExceptionSensibly :: SlackConfig -> SomeException -> Int -> IO Bool
handleThreadExceptionSensibly cfg ex tId
  = runStdoutLoggingT
  . withLogLevel (logLevel cfg)
  . withThreadContext ["threadId" .= tId] $ do
    logError $ "thread exception" :# ["error" .= show ex]
    case fromException ex of
      Just (ConnectionError (fromException -> (Just (IOError { ioe_type = ResourceVanished })))) ->
        pure True
      Just (ConnectionError (fromException -> (Just (IOError { ioe_type = TimeExpired })))) ->
        pure True
      Just (ConnectionError (fromException -> Just WS.ConnectionClosed)) ->
        pure True
      Just (JSONDecodeError _) ->
        pure True
      _ ->
        pure False

shutdownSocketMode :: SocketModeEnv -> IO ()
shutdownSocketMode env = atomically $ putTMVar (shutdownVar env) ()

-- | Cancel all background threads, wait for them to finish,
-- and then close the event queue.
spawnShutdownHandler :: SocketModeEnv -> IO (Async ())
spawnShutdownHandler env = asyncWithUnmask $ \_ ->
  runStdoutLoggingT . withLogLevel (logLevel $ slackConfig env) $ do
    atomically . takeTMVar $ shutdownVar env
    logDebug "shutting down"
    threads <- fmap fst <$> readTVarIO (bgThreads env)
    uninterruptibleMask_ $ traverse_ uninterruptibleCancel threads
    traverse_ waitCatch threads
    atomically $ closeTBMQueue (inboundQueue env)

-- | Requests a new socket connection from Slack, then blocks on receiving data from that connection.
-- A parsed event is ack'd immediately and inserted into a bounded queue.
-- Connection refreshes are handled by letting the inner loop return, which causes
-- the outer loop to create a fresh connection.
-- This should never return unless an IO exception is thrown from the inner or outer loops.
pollSocket :: SocketModeEnv -> Int -> IO ()
pollSocket env tId
  = runStdoutLoggingT
  . withLogLevel (logLevel cfg)
  . withThreadContext ["threadId" .= tId] $ do
    atomically . readTMVar $ startVar env
    logDebug "opening connection"
    url <- connectionsOpen env
    (host, path) <- parseWebSocketUrl url
    liftIO $ WS.runSecureClient host 443 path go
    liftIO $ pollSocket env tId
  where
    cfg = slackConfig env
    lock = refreshLock env
    inboundQ = inboundQueue env
    go conn
      = runStdoutLoggingT
      . withLogLevel (logLevel cfg)
      . withThreadContext ["threadId" .= tId] $ do
        logDebug "connection receiving data"
        liftIO $ WS.withPingThread conn 15 (pure ()) (loop conn)
    writeInboundEvent conn val = case val of
      EventsApi (EventsApiEnvelope { eaeEnvelopeId = eId}) -> do
        ackEnvelopeId conn eId
        atomically $ writeTBMQueue inboundQ val
        loop conn
      Hello _ -> do
        void . atomically $ tryTakeTMVar lock
        atomically $ writeTBMQueue inboundQ val
        loop conn
      Disconnect _ -> do
        -- Slack sends a warning about 10 seconds before a refresh request.
        -- This gives us enough time to refresh one connection at a time,
        -- synchronized via the refresh lock (TMVar).
        atomically $ writeTBMQueue inboundQ val
        unlocked <- atomically $ tryPutTMVar lock ()
        runStdoutLoggingT . withLogLevel (logLevel cfg) $ logDebug "connection refresh requested"
        if unlocked then pure () else loop conn
      -- TODO Need to handle ack payloads for Interactive and SlashCommands
      Interactive _   -> loop conn
      SlashCommands _ -> loop conn
    loop conn = do
      raw <- WS.receiveData conn `catch` (throwIO . ConnectionError)
      either (throwIO . JSONDecodeError) (writeInboundEvent conn) $ Aeson.eitherDecode raw

-- | Given a function that can be run in any monad on top of IO, pull an event
-- from the queue and apply the function to it, then loop. This blocks on getting
-- the next event, and the loop is broken when the queue is empty and closed.
handleEvents
  :: MonadIO m
  => SocketModeEnv
  -> (SlackConfig -> SocketModeEvent -> m ())
  -> m ()
handleEvents env fn = getNextEvent env >>= \case
  Just evt -> fn cfg evt >> handleEvents env fn
  Nothing  -> pure ()
  where
    cfg = slackConfig env

-- | Returns Nothing when the inbound queue is closed, meaning all
-- listener threads have been shut down.
getNextEvent :: MonadIO m => SocketModeEnv -> m (Maybe SocketModeEvent)
getNextEvent = liftIO . atomically . readTBMQueue . inboundQueue

data AckPayload =
  AckPayload
    { envelopeId :: !Text
    , payload    :: !(Maybe Aeson.Value)
    }
  deriving stock (Generic, Eq, Show)

instance Aeson.ToJSON AckPayload where
  toJSON
    = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    , Aeson.omitNothingFields = True
    }

ackEnvelopeId :: WS.Connection -> Text -> IO ()
ackEnvelopeId conn eId = WS.sendTextData conn . toJSONText $ AckPayload eId Nothing

connectionsOpen :: MonadIO m => SocketModeEnv -> m Text
connectionsOpen env = do
  let appToken = slackAppToken . slackConfig $ env
      debug = debugDisconnect . slackConfig $ env
  resp <- makeSlackPostJSONNoBody appToken "apps.connections.open"
  url <- (resp ^? key "url" . _String)
          `assertJust` "apps.connections.open: No url!"
  pure $ if debug then url <> "&debug_reconnects=true" else url

parseWebSocketUrl :: MonadIO m => T.Text -> m (String, String)
parseWebSocketUrl url = do
  uri  <- URI.parseURI (T.unpack url) `assertJust` ("Couldn't parse WebSockets URL: " <> url)
  auth <- URI.uriAuthority uri `assertJust` ("No authority: " <> url)
  pure (URI.uriRegName auth, URI.uriPath uri <> URI.uriQuery uri)
