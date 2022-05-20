module Slacker.Config
  ( SlackConfig(..)
  , defaultSlackConfig
  , setApiToken
  , setAppToken
  , setDebugDisconnect
  , setGracefulShutdownHandler
  , setNumThreads
  , setInboundQueueMax
  , setOnException
  , setLogLevel
  , withLogLevel
  ) where

import           Control.Monad.Logger.Aeson
  ( LogLevel(..)
  , LoggingT
  , Message(..)
  , filterLogger
  , logError
  , runStdoutLoggingT
  , (.=)
  )
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           UnliftIO.Exception (SomeException)

data SlackConfig
  = SlackConfig
  { slackApiToken   :: !Text
  -- ^ OAuth token used to access the Slack web API
  , slackAppToken   :: !Text
  -- ^ App token with connection scope to initiate websocket session
  , numThreads      :: !Int
  -- ^ Minimum number of subcriber threads to keep open. Defaults to 2. Max of 10.
  , debugDisconnect :: !Bool
  -- ^ When true, forces connection refreshes every 360 seconds.
  , inboundQueueMax :: !Int
  -- ^ Maximum amount of work to keep queued up. Setting this higher
  -- can increase memory usage, but means threads are less likely to drop events during high traffic.
  -- Slack retries messages up to 3 times, the last attempt occurring after 5 minutes.
  -- Larger queues can also lead to longer graceful shutdown times if your app experiences high event traffic.
  , gracefulShutdownHandler :: IO () -> IO ()
  -- ^ This handler is passed a function that initiates a close of all connection threads
  -- before finally closing the queue. This allows for a more graceful shutdown when,
  -- for example, you intercept an OS kill signal.
  , onException :: SlackConfig -> SomeException -> Int -> IO Bool
  -- ^ When a socket polling thread throws an exception you can return True from this callback if you want
  -- the thread to be restarted. Returning False will initiate a graceful shutdown of socket mode.
  -- The default is to log the error and shut down.
  , logLevel :: Maybe LogLevel
  -- ^ The monad-logger LogLevel to use for the structured logs. All logs are printed to stdout.
  -- Set this to Nothing if you want to disable the logs from the daemon.
  -- Default is `LevelInfo`.
  } deriving stock (Generic)

defaultSlackConfig :: SlackConfig
defaultSlackConfig
  = SlackConfig
  { slackApiToken           = ""
  , slackAppToken           = ""
  , numThreads              = 2
  , debugDisconnect         = False
  , inboundQueueMax         = 1000
  , gracefulShutdownHandler = const $ pure ()
  , onException             = defaultOnException
  , logLevel                = Just LevelInfo
  }

setApiToken :: Text -> SlackConfig -> SlackConfig
setApiToken apiT sc = sc { slackApiToken = apiT }

setAppToken :: Text -> SlackConfig -> SlackConfig
setAppToken appT sc = sc { slackAppToken = appT }

setDebugDisconnect :: Bool -> SlackConfig -> SlackConfig
setDebugDisconnect dd sc = sc { debugDisconnect = dd }

-- | Slack only allows 10 open socket connections per app.
-- We recommend at least 2 connections open to prevent dropping
-- events during a connection refresh.
setNumThreads :: Int -> SlackConfig -> SlackConfig
setNumThreads nt sc = sc { numThreads = min 10 nt }

setInboundQueueMax :: Int -> SlackConfig -> SlackConfig
setInboundQueueMax n sc = sc { inboundQueueMax = n }

setGracefulShutdownHandler :: (IO () -> IO ()) -> SlackConfig -> SlackConfig
setGracefulShutdownHandler h sc = sc { gracefulShutdownHandler = h }

setOnException :: (SlackConfig -> SomeException -> Int -> IO Bool) -> SlackConfig -> SlackConfig
setOnException oe sc = sc { onException = oe }

setLogLevel :: Maybe LogLevel -> SlackConfig -> SlackConfig
setLogLevel lvl sc = sc { logLevel = lvl }

-- | Logs the error and initiates a graceful shuts down.
defaultOnException :: SlackConfig -> SomeException -> Int -> IO Bool
defaultOnException cfg e _tId = runStdoutLoggingT . withLogLevel (logLevel cfg) $ do
  logError $ "thread exception" :# ["error" .= show e]
  pure False

withLogLevel :: Maybe LogLevel -> LoggingT m a -> LoggingT m a
withLogLevel Nothing = filterLogger (\_ _ -> False)
withLogLevel (Just loglvl) = filterLogger (\_ lvl -> lvl >= loglvl)

