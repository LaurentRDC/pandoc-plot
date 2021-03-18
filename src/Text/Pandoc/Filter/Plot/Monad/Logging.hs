{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  ( Verbosity (..),
    LogSink (..),
    Logger (..),
    withLogger,
    terminateLogging,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, void)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import Data.Text.IO as TIO (appendFile, hPutStr)
import Data.Yaml (FromJSON (parseJSON), Value (String))
import System.IO (stderr)

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
  { lVerbosity :: Verbosity,
    lChannel :: Chan (Maybe Text),
    lSink :: Text -> IO (),
    lSync :: MVar ()
  }

-- | Ensure that all log messages are flushed, and stop logging
terminateLogging :: Logger -> IO ()
terminateLogging logger = do
  -- Flushing the logger
  -- To signal to the logger that logging duties are over,
  -- we append Nothing to the channel, and wait for it to finish
  -- dealing with all items in the channel.
  writeChan (lChannel logger) Nothing
  void $ takeMVar (lSync logger)

-- | Perform an IO action with a logger. Using this function
-- ensures that logging will be gracefully shut down.
withLogger :: Verbosity -> LogSink -> (Logger -> IO a) -> IO a
withLogger v s f = do
  logger <-
    Logger <$> pure v
      <*> newChan
      <*> pure (sink s)
      <*> newEmptyMVar

  -- The logger either logs messages (if Just "message"),
  -- or stops working on Nothing.
  _ <-
    forkIO $
      forever $
        readChan (lChannel logger)
          >>= maybe (putMVar (lSync logger) ()) (lSink logger)

  result <- f logger

  terminateLogging logger

  return result
  where
    sink :: LogSink -> Text -> IO ()
    sink StdErr = TIO.hPutStr stderr
    sink (LogFile fp) = TIO.appendFile fp

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
        intercalate ", " $
          fmap (fmap toLower . show) $
            enumFromTo minBound (maxBound :: Verbosity)

instance FromJSON Verbosity where
  parseJSON (String t) = pure $ fromString . unpack $ t
  parseJSON _ = fail "Could not parse the logging verbosity."
