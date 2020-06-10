{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Logging implementation. Inspired by the Hakyll implementation.
-}

module Text.Pandoc.Filter.Plot.Logging 
    ( Verbosity(..)
    , Logger
    , LogSink(..)
    , withLogger
    -- * Logging messages
    , debug
    , err
    , warning
    , info
    ) where


import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Control.Monad.Trans     (MonadIO, liftIO)

import           Data.Char               (toLower)
import           Data.String             (IsString(..))
import           Data.Text               (Text, unpack)
import qualified Data.Text.IO            as TIO
import           Data.Yaml

import           System.IO               (stderr)


-- | Verbosity of the logger.
data Verbosity = Silent   -- ^ Don't log anything.
               | Info     -- ^ Only log information messages.
               | Warning  -- ^ Log information and warning messages.
               | Error    -- ^ Log information, warnings, and errors.
               | Debug    -- ^ Log all messages, including debug messages.
               deriving (Eq, Ord, Show)

instance IsString Verbosity where
    fromString s
        | ls == "silent"  = Silent
        | ls == "info"    = Info
        | ls == "warning" = Warning
        | ls == "error"   = Error
        | ls == "debug"   = Debug
        | otherwise = error $ "Unrecognized verbosity " <> s
        where
            ls = toLower <$> s

instance FromJSON Verbosity where
    parseJSON (String t) = pure $ fromString . unpack $ t
    parseJSON _ = fail $ "Could not parse the logging verbosity."


-- | Description of the possible ways to sink log messages.
data LogSink = StdErr           -- ^ Standard error stream.
             | LogFile FilePath -- ^ Appended to file.
             deriving (Eq, Show)


-- | Logger type.
--
-- The recommended usage of @Logger@ is to use the function @withLogger@. This ensures
-- that the logger is properly flushed after use.
data Logger = Logger
    { logVerbosity :: Verbosity         -- ^ Logger verbosity.
    , logChannel   :: Chan (Maybe Text) -- ^ Logger channel.
    , logOp        :: Text -> IO ()     -- ^ Logging operation (e.g. @putStrLn@, or @writeFile@).
    , logSync      :: MVar ()           -- ^ Syncing variable.
    }


-- | Create a logger and run an IO action. This function ensures that the
-- logger is properly flushed.
withLogger :: Verbosity -> LogSink -> (Logger -> IO ()) -> IO ()
withLogger v sink f = do
    logger <- new v sink
    f logger
    flush logger


-- | Create a new logger.
new :: Verbosity -> LogSink -> IO Logger
new v sink = do
    logger <- Logger <$> pure v <*> newChan <*> pure (logsink sink) <*> newEmptyMVar
    _ <- forkIO $ loggerThread logger
    return logger
    where
        logsink StdErr       = TIO.hPutStrLn stderr
        logsink (LogFile fp) = TIO.appendFile fp

        loggerThread Logger{..} = forever $ do
            msg <- readChan logChannel
            case msg of 
                Nothing -> putMVar logSync ()
                Just t  -> logOp t


-- | Flush the content of the logger.
flush :: Logger -> IO ()
flush Logger{..} = do
    writeChan logChannel Nothing
    () <- takeMVar logSync
    return ()


text :: MonadIO m => Logger -> Verbosity -> Text -> m ()
text Logger{..} v t 
    | (logVerbosity >= v) = liftIO $ writeChan logChannel (Just t)
    | otherwise           = return ()


debug :: MonadIO m => Logger -> Text -> m ()
debug l t = text l Debug $ "(DEBUG)   " <> t


err :: MonadIO m => Logger -> Text -> m ()
err l t = text l Error $ "(ERROR)   " <> t


warning :: MonadIO m => Logger -> Text -> m ()
warning l t = text l Warning $ "(WARNING) " <> t


info :: MonadIO m => Logger -> Text -> m ()
info l t = text l Info $ "          " <> t