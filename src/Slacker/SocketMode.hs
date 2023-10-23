module Slacker.SocketMode
  ( module Export
  , SocketModeEnv(..)
  , ThreadError(..)
  , getNextEvent
  , handleEvents
  , handleThreadExceptionSensibly
  , initSocketMode
  , runSocketMode
  , shutdownSocketMode
  ) where

import           Control.Concurrent.STM.TBMQueue
import           Control.Monad (void)
import           Control.Monad.IO.Unlift (MonadIO, liftIO)
import           Control.Monad.Logger.Aeson
import qualified Data.Aeson as Aeson
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import           GHC.IO.Exception
import           Lens.Micro ((^?))
import           Lens.Micro.Aeson (_String, key)
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

import           Slacker.Config (SlackConfig(..), logStdout, logThread)
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

-- | Subscribe to events using Socket Mode and handle them
-- with the given continuation. This function is intended to be run
-- as a daemon, so it is recommended to catch exceptions in your
-- event handler and handle OS kill signals with `gracefulShutdownHandler`.
runSocketMode :: MonadIO m => SlackConfig -> (SlackConfig -> SocketModeEvent -> m ()) -> m ()
runSocketMode cfg h = liftIO (initSocketMode cfg) >>= flip handleEvents h

-- | Create a `SocketModeEnv` from a `SlackConfig`.
initSocketMode :: SlackConfig -> IO SocketModeEnv
initSocketMode cfg = mask_ . logStdout cfg $ do
  logDebug "setting up"
  iqueue      <- liftIO $ newTBMQueueIO (inboundQueueMax cfg)
  tsTVar      <- liftIO $ newTVarIO []
  refLock     <- liftIO newEmptyTMVarIO
  shutdownVar <- liftIO newEmptyTMVarIO
  startVar    <- liftIO newEmptyTMVarIO
  let env = SocketModeEnv cfg iqueue tsTVar refLock shutdownVar startVar
  threads <- for [1..numThreads cfg] $ \tId -> do
    t <- liftIO $ asyncWithUnmask (\um -> um $ pollSocket env tId)
    pure (t, tId)
  void . atomically $ swapTVar tsTVar threads
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
        Left err | Just AsyncCancelled <- fromException err
                 , Just tId <- lookup stoppedAsync threads -> do
          logThread cfg tId $ logDebug "thread canceled"
          let newThreads = filter ((/= tId) . snd) threads
          void . atomically $ swapTVar (bgThreads env) newThreads
          loop
        Left err | Just tId <- lookup stoppedAsync threads -> do
          shouldRestart <- onException cfg cfg err tId
          if shouldRestart
            then restartFailed env threads tId >> loop
            else shutdownSocketMode env
        _ -> do
          -- Polling thread exited normally, which shouldn't ever happen.
          logStdout cfg $ logError "a thread exited without exception, we're shutting it all down"
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
handleThreadExceptionSensibly cfg ex tId = do
  logThread cfg tId . logError $ "thread exception" :# ["error" .= show ex]
  case fromException ex of
    Just (ConnectionError (fromException -> (Just (IOError { ioe_type = ResourceVanished })))) ->
      pure True
    Just (ConnectionError (fromException -> (Just (IOError { ioe_type = TimeExpired })))) ->
      pure True
    Just (ConnectionError (fromException -> Just WS.ConnectionClosed)) ->
      pure True
    Just (ConnectionError (fromException -> Just (WS.ParseException _))) ->
      pure True
    Just (JSONDecodeError _) ->
      pure True
    -- Slack sometimes returns 408 responses that should be recoverable
    Just (ConnectionError (fromException -> Just (WS.MalformedResponse rh _))) | WS.responseCode rh == 408 ->
      pure True
    _ ->
      pure False

-- | Initiates a graceful shutdown.
shutdownSocketMode :: SocketModeEnv -> IO ()
shutdownSocketMode env = atomically $ putTMVar (shutdownVar env) ()

-- | Spawns a thread that waits for the shutdown signal.
-- When signaled, this cancels all socket threads, waits for them to finish,
-- and then closes the event queue.
spawnShutdownHandler :: SocketModeEnv -> IO (Async ())
spawnShutdownHandler env = asyncWithUnmask $ \_ -> do
  atomically . takeTMVar $ shutdownVar env
  logStdout (slackConfig env) $ logDebug "shutting down"
  threads <- fmap fst <$> readTVarIO (bgThreads env)
  uninterruptibleMask_ $ traverse_ uninterruptibleCancel threads
  traverse_ waitCatch threads
  atomically $ closeTBMQueue (inboundQueue env)

-- | Requests a new socket connection from Slack and blocks on receiving data from that connection.
-- A socket mode envelope id is used to ack the message immediately, after which the entire value
-- is inserted into the bounded, closeable event queue.
-- Connection refreshes are handled by letting the inner loop return, which causes
-- the outer loop to create a fresh connection.
-- This should never return unless an IO exception is thrown from the inner or outer loops.
pollSocket :: SocketModeEnv -> Int -> IO ()
pollSocket env tId = logThread cfg tId $ do
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
    go conn = do
      logStdout cfg $ logDebug "connection receiving data"
      liftIO $ WS.withPingThread conn 15 (pure ()) (loop conn)
    writeInboundEvent conn val = case val of
      EventsApi (EventsApiEnvelope { eaeEnvelopeId = eId}) -> do
        ackEnvelopeId conn eId
        atomically $ writeTBMQueue inboundQ val
        loop conn
      SlashCommands (SlashCommandsEnvelope { sceEnvelopeId = eId }) -> do
        ackEnvelopeId conn eId
        atomically $ writeTBMQueue inboundQ val
        loop conn
      InteractiveEvent (InteractiveEnvelope { ieEnvelopeId = eId }) -> do
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
        logStdout cfg $ logDebug "connection refresh requested"
        if unlocked then pure () else loop conn
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

-- | Returns Nothing when the inbound queue is closed and empty.
getNextEvent :: MonadIO m => SocketModeEnv -> m (Maybe SocketModeEvent)
getNextEvent = liftIO . atomically . readTBMQueue . inboundQueue

ackEnvelopeId :: WS.Connection -> Text -> IO ()
ackEnvelopeId conn eId =
  WS.sendTextData conn (toJSONText $ AckPayload eId Nothing)
    `catch` (throwIO . ConnectionError)

connectionsOpen :: MonadIO m => SocketModeEnv -> m Text
connectionsOpen env = do
  let appToken = slackAppToken . slackConfig $ env
      debug = debugDisconnect . slackConfig $ env
  resp <- makeSlackPostJSONNoBody appToken "apps.connections.open"
  url <- (resp ^? key "url" . _String) `assertJust` "apps.connections.open: No url!"
  pure $ if debug then url <> "&debug_reconnects=true" else url

parseWebSocketUrl :: MonadIO m => T.Text -> m (String, String)
parseWebSocketUrl url = do
  uri  <- URI.parseURI (T.unpack url) `assertJust` ("Couldn't parse WebSockets URL: " <> url)
  auth <- URI.uriAuthority uri `assertJust` ("No authority: " <> url)
  pure (URI.uriRegName auth, URI.uriPath uri <> URI.uriQuery uri)
