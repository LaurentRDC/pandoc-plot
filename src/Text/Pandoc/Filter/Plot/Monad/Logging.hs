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
    , Logger(..)
    , withLogger
    ) where


import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan      (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar, takeMVar)

import           Control.Monad                (forever)

import           Data.Char                    (toLower)
import           Data.List                    (intercalate)
import           Data.String                  (IsString(..))
import           Data.Text                    (Text, unpack)
import           Data.Text.IO                 (hPutStr)
import           Data.Yaml

import           System.IO                    (stderr, withFile, IOMode (AppendMode) )



-- | Verbosity of the logger.
data Verbosity = Debug    -- ^ Log all messages, including debug messages.
               | Info     -- ^ Log information, warning, and error messages.
               | Warning  -- ^ Log warning and error messages.
               | Error    -- ^ Only log errors.
               | Silent   -- ^ Don't log anything. 
               deriving (Eq, Ord, Show, Enum, Bounded)


-- | Description of the possible ways to sink log messages.
data LogSink = StdErr           -- ^ Standard error stream.
             | LogFile FilePath -- ^ Appended to file.
             deriving (Eq, Show)


-- | The logging implementation is very similar to Hakyll's.
data Logger = Logger
    { lVerbosity :: Verbosity
    , lChannel   :: Chan (Maybe Text)
    , lSink      :: Text -> IO ()
    , lSync      :: MVar ()
    }


-- | Perform an IO action with a logger. Using this function
-- ensures that logging will be gracefully shut down.
withLogger :: Verbosity -> LogSink -> (Logger -> IO a) -> IO a
withLogger v s f = do
    logger <- Logger <$> pure v
                     <*> newChan
                     <*> pure (sink s)
                     <*> newEmptyMVar 

    -- The logger either logs messages (if Just "message"),
    -- or stops working on Nothing.
    _ <- forkIO $ forever $ 
            readChan (lChannel logger) 
                >>= maybe (putMVar (lSync logger) ()) (lSink logger)

    result <- f logger
    
    -- Flushing the logger
    -- To signal to the logger that logging duties are over,
    -- we append Nothing to the channel, and wait for it to finish
    -- dealing with all items in the channel.
    writeChan (lChannel logger) Nothing
    () <- takeMVar (lSync logger)

    return result

    where
        sink StdErr       = hPutStr stderr
        sink (LogFile fp) = \t -> withFile fp AppendMode $ \h -> hPutStr h t


instance IsString Verbosity where
    fromString s
        | ls == "silent"  = Silent
        | ls == "info"    = Info
        | ls == "warning" = Warning
        | ls == "error"   = Error
        | ls == "debug"   = Debug
        | otherwise = errorWithoutStackTrace $ mconcat ["Unrecognized verbosity '", s, "'. Valid choices are: " ] <> choices
        where
            ls = toLower <$> s
            choices = intercalate ", " 
                    $ fmap (fmap toLower . show) 
                    $ enumFromTo minBound (maxBound::Verbosity)

instance FromJSON Verbosity where
    parseJSON (String t) = pure $ fromString . unpack $ t
    parseJSON _ = fail $ "Could not parse the logging verbosity."