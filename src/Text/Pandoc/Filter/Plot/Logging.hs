{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Logging implementation. Inspired by Hakyll and monad-logger.
-}

module Text.Pandoc.Filter.Plot.Logging 
    ( Verbosity(..)
    , LogSink(..)
    , LoggingT(..)
    -- * Logging messages
    , debug
    , err
    , warning
    , info
    ) where


import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever, mapM_)
import           Control.Monad.Trans     (MonadIO, liftIO)
import           Control.Monad.Writer    (WriterT, runWriterT, tell)

import           Data.Char               (toLower)
import           Data.String             (IsString(..))
import           Data.Text               (Text, unpack)
import qualified Data.Text.IO            as TIO
import           Data.Yaml

import           System.IO               (stderr)

import           Prelude                 hiding (log)


-- | Verbosity of the logger.
data Verbosity = Silent   -- ^ Don't log anything.
               | Info     -- ^ Only log information messages.
               | Warning  -- ^ Log information and warning messages.
               | Error    -- ^ Log information, warnings, and errors.
               | Debug    -- ^ Log all messages, including debug messages.
               deriving (Eq, Ord, Show)


-- | Description of the possible ways to sink log messages.
data LogSink = StdErr           -- ^ Standard error stream.
             | LogFile FilePath -- ^ Appended to file.
             deriving (Eq, Show)


type LogMessage = (Verbosity, Text)

type LoggingT m a = WriterT [LogMessage] m a


runLoggingT :: MonadIO m => Verbosity -> LogSink -> LoggingT m a -> m a
runLoggingT v StdErr       = runLoggingT' v $ mapM_ (TIO.hPutStrLn stderr . snd)
runLoggingT v (LogFile fp) = runLoggingT' v $ mapM_ (TIO.appendFile fp . snd)


runLoggingT' :: MonadIO m 
            => Verbosity                -- ^ Minimum verbosity to keep
            -> ([LogMessage] -> IO ())  -- ^ Log sink
            -> LoggingT m a
            -> m a
runLoggingT' v f m = do
    (r, t) <- runWriterT m
    -- Messages with lower level than minimum are discarded
    let t' = filter (\message -> fst message >= v) t
    liftIO $ f t'
    return r


log :: Monad m => Verbosity -> Text -> LoggingT m ()
log v t = tell [(v, t)]


debug :: Monad m => Text -> LoggingT m ()
debug t = log Debug $ "(DEBUG)   " <> t


err :: Monad m => Text -> LoggingT m ()
err t = log Error $ "(ERROR)   " <> t


warning :: Monad m => Text -> LoggingT m ()
warning t = log Warning $ "(WARNING) " <> t


info :: Monad m => Text -> LoggingT m ()
info t = log Info $ "          " <> t


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