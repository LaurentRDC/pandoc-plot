{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Logging primitives.
module Text.Pandoc.Filter.Plot.Monad.Logging
  ( MonadLogger (..),
    Verbosity (..),
    LogSink (..),
    Logger (..),
    withLogger,
    terminateLogging,
    -- * Logging messages
    debug,
    err,
    warning,
    info,
    strict,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, void, when, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.IO as TIO (appendFile, hPutStr)
import Data.Yaml (FromJSON (parseJSON), Value (String))
import System.IO (stderr)
import Prelude hiding (log)

-- | Verbosity of the logger.
data Verbosity
  = -- | Log all messages, including debug messages.
    Debug
  | -- | Log information, warning, and error messages.
    Info
  | -- | Log warning and error messages.
    Warning
  | -- | Only log errors.
    Error
  | -- | Don't log anything.
    Silent
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Description of the possible ways to sink log messages.
data LogSink
  = -- | Standard error stream.
    StdErr
  | -- | Appended to file.
    LogFile FilePath
  deriving (Eq, Show)

-- | The logging implementation is very similar to Hakyll's.
data Logger = Logger
  { lVerbosity :: Verbosity,  -- Verbosity level below which to ignore messages
    lChannel :: Chan Command, -- Queue of logging commands
    lSink :: Text -> IO (),   -- Action to perform with log messages
    lSync :: MVar ()          -- Synchronization variable
  }

data Command 
  = LogMessage Text 
  | EndLogging

class Monad m => MonadLogger m where
  askLogger :: m Logger

-- | Ensure that all log messages are flushed, and stop logging
terminateLogging :: Logger -> IO ()
terminateLogging logger = do
  -- Flushing the logger
  -- To signal to the logger that logging duties are over,
  -- we append Nothing to the channel, and wait for it to finish
  -- dealing with all items in the channel.
  writeChan (lChannel logger) EndLogging
  void $ takeMVar (lSync logger)

-- | Perform an IO action with a logger. Using this function
-- ensures that logging will be gracefully shut down.
withLogger :: Verbosity -> LogSink -> (Logger -> IO a) -> IO a
withLogger v s f = do
  logger <-
    Logger v
      <$> newChan
      <*> pure (sink s)
      <*> newEmptyMVar

  -- The logger either logs messages (if Just "message"),
  -- or stops working on Nothing.
  _ <-
    forkIO $
      forever $
        readChan (lChannel logger) 
          >>= \case
            EndLogging -> putMVar (lSync logger) ()
            LogMessage t -> lSink logger t

  result <- f logger

  terminateLogging logger

  return result
  where
    sink :: LogSink -> Text -> IO ()
    sink StdErr = TIO.hPutStr stderr
    sink (LogFile fp) = TIO.appendFile fp
    

-- | General purpose logging function.
log :: (MonadLogger m, MonadIO m) 
  => Text   -- Header
  -> Verbosity 
  -> Text 
  -> m ()
log h v t = do
  logger <- askLogger
  when (v >= lVerbosity logger) $
    liftIO $ do
      forM_ (T.lines t) $ \l -> writeChan (lChannel logger) (LogMessage (h <> l <> "\n"))


debug, err, strict, warning, info :: (MonadLogger m, MonadIO m) => Text -> m ()
debug = log "[pandoc-plot] DEBUG | " Debug
err = log "[pandoc-plot] ERROR | " Error
strict = log "[pandoc-plot] STRICT MODE | " Error
warning = log "[pandoc-plot] WARN  | " Warning
info = log "[pandoc-plot] INFO  | " Info


instance IsString Verbosity where
  fromString s
    | ls == "silent" = Silent
    | ls == "info" = Info
    | ls == "warning" = Warning
    | ls == "error" = Error
    | ls == "debug" = Debug
    | otherwise = errorWithoutStackTrace $ mconcat ["Unrecognized verbosity '", s, "'. Valid choices are: "] <> choices
    where
      ls = toLower <$> s
      choices =
        intercalate
          ", "
          ( fmap toLower . show
              <$> enumFromTo minBound (maxBound :: Verbosity)
          )

instance FromJSON Verbosity where
  parseJSON (String t) = pure $ fromString . unpack $ t
  parseJSON _ = fail "Could not parse the logging verbosity."
