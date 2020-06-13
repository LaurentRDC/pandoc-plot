{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Logging primitives.
-}

module Text.Pandoc.Filter.Plot.Monad.Logging 
    ( Verbosity(..)
    , LogSink(..)
    , LoggingM
    , runLoggingM
    , log
    ) where


import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Writer.Strict (WriterT, runWriterT, tell)

import           Data.Char                   (toLower)
import           Data.List                   (sortOn)
import           Data.String                 (IsString(..))
import           Data.Text                   (Text, unpack)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Time.Clock.System      (getSystemTime, SystemTime(..))
import           Data.Yaml

import           System.IO                   (stderr)

import           Prelude                     hiding (log, fst, snd)


-- | Verbosity of the logger.
data Verbosity = Debug    -- ^ Log all messages, including debug messages.
               | Error    -- ^ Log information, warnings, and errors.
               | Warning  -- ^ Log information and warning messages.
               | Info     -- ^ Only log information messages.
               | Silent   -- ^ Don't log anything. 
               deriving (Eq, Ord, Show)


-- | Description of the possible ways to sink log messages.
data LogSink = StdErr           -- ^ Standard error stream.
             | LogFile FilePath -- ^ Appended to file.
             deriving (Eq, Show)

type LogMessage = (Verbosity, SystemTime, Text)

type LoggingM = WriterT [LogMessage] IO


runLoggingM :: Verbosity -> LogSink -> LoggingM a -> IO a
runLoggingM Silent _       = runLoggingM' Silent $ mapM_ (return . trd)
runLoggingM v StdErr       = runLoggingM' v $ mapM_ (TIO.hPutStrLn stderr . trd)
runLoggingM v (LogFile fp) = runLoggingM' v $ mapM_ (TIO.appendFile fp . trd)


runLoggingM' :: Verbosity                -- ^ Minimum verbosity to keep
             -> ([LogMessage] -> IO ())  -- ^ Log sink
             -> LoggingM a
             -> IO a
runLoggingM' v f m = do
    (r, t) <- runWriterT m
    -- Messages with lower level than minimum are discarded
    -- We also re-order messages to be chronological
    let t' = sortOn snd $ filter (\message -> fst message >= v) t
    liftIO $ f t'
    return r


-- | General logging function.
-- Input text will be decomposed into lines, with each
-- line becoming a log line.    
log :: Text       -- ^ Header  
    -> Verbosity  
    -> Text       -- ^ Message
    -> LoggingM ()
log h v t = do
    timestamp <- liftIO $ getSystemTime
    tell [(v, timestamp, h <> l <> newline) | l <- T.lines t]


newline :: Text
newline = "\n"


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

fst :: (a,b,c) -> a
fst (a,_,_) = a

snd :: (a,b,c) -> b
snd (_,b,_) = b

trd :: (a,b,c) -> c
trd (_,_,c) = c