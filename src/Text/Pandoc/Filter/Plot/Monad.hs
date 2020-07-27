{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines the @PlotM@ monad and related capabilities.
-}

module Text.Pandoc.Filter.Plot.Monad (
      Configuration(..)
    , PlotM
    , RuntimeEnv(..)
    , runPlotM
    -- * Running external commands
    , runCommand
    -- * Logging
    , Verbosity(..)
    , LogSink(..)
    , debug
    , err
    , warning
    , info
    -- * Lifting and other monadic operations
    , liftIO
    , ask
    , asks
    , asksConfig
    , silence
    -- * Base types
    , module Text.Pandoc.Filter.Plot.Monad.Types
) where


import           Control.Concurrent.Chan     (writeChan)

import           Control.Monad.Reader

import           Data.ByteString.Lazy        (toStrict)
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8With)
import           Data.Text.Encoding.Error    (lenientDecode)

import           System.Exit                 (ExitCode (..))
import           System.Process.Typed        ( readProcessStderr, shell, nullStream
                                             , setStdout, setStderr, byteStringOutput
                                             )

import           Text.Pandoc.Definition      (Format(..))

import           Prelude                     hiding (log, fst, snd)

import Text.Pandoc.Filter.Plot.Monad.Logging as Log
import Text.Pandoc.Filter.Plot.Monad.Types


-- | pandoc-plot monad
type PlotM a = ReaderT RuntimeEnv IO a


data RuntimeEnv = 
    RuntimeEnv { envConfig :: Configuration
               , envLogger :: Logger
               }


-- | Modify the runtime environment to be silent.
silence :: PlotM a -> PlotM a
silence = local (\(RuntimeEnv c l) -> RuntimeEnv c l{lVerbosity = Silent})


-- | Get access to configuration within the @PlotM@ monad.
asksConfig :: (Configuration -> a) -> PlotM a
asksConfig f = asks (f . envConfig)


-- | Evaluate a @PlotM@ action.
runPlotM :: Configuration -> PlotM a -> IO a
runPlotM conf v = 
    let verbosity = logVerbosity conf
        sink      = logSink conf 
    in withLogger verbosity sink $ 
        \logger -> runReaderT v (RuntimeEnv conf logger)


debug, err, warning, info :: Text -> PlotM ()
debug     = log "DEBUG| " Debug 
err       = log "ERROR| " Error 
warning   = log "WARN | " Warning 
info      = log "INFO | " Info 


-- | General purpose logging. 
log :: Text      -- ^ Header.
    -> Verbosity -- ^ Verbosity of the message.
    -> Text      -- ^ Message (can be multiple lines).
    -> PlotM ()
log h v t = do
    logger <- asks envLogger
    when (v >= lVerbosity logger) $ 
        liftIO $ do
            let lines' = [l' | l' <- T.lines t]
            forM_ lines' $ \l -> writeChan (lChannel logger) (Just (h <> l <> "\n"))


-- | Run a command within the @PlotM@ monad. Stderr stream
-- is read and decoded, while Stdout is ignored. 
-- Logging happens at the debug level if the command succeeds, or at
-- the error level if it does not succeed..
runCommand :: Text -> PlotM (ExitCode, Text)
runCommand command = do
    (ec, processOutput') <- liftIO 
                        $ readProcessStderr 
                        $ setStdout nullStream
                        $ setStderr byteStringOutput 
                        $ shell (unpack command)
    let processOutput = decodeUtf8With lenientDecode $ toStrict processOutput'
        logFunc = if ec == ExitSuccess
                    then debug
                    else err
        message = T.unlines [ "Running command"
                            , "    " <> command
                            , "ended with exit code " <> (pack . show $ ec)
                            ] 
        errorMessage = if processOutput == mempty 
            then mempty 
            else T.unlines [ "*******"
                           , processOutput
                           , "*******"
                           ]
    
    logFunc $ message <> errorMessage
    return (ec, processOutput)


-- | The @Configuration@ type holds the default values to use
-- when running pandoc-plot. These values can be overridden in code blocks.
--
-- You can create an instance of the @Configuration@ type from file using the @configuration@ function.
--
-- You can store the path to a configuration file in metadata under the key @plot-configuration@. For example, in Markdown:
--
-- @
--     ---
--     title: My document
--     author: John Doe
--     plot-configuration: path\to\file.yml
--     ---     
-- @
--
-- The same can be specified via the command line using Pandoc's @-M@ flag:
--
-- > pandoc --filter pandoc-plot -M plot-configuration="path/to/file.yml" ...
--
-- In this case, use @configurationPathMeta@ to extact the path from @Pandoc@ documents.
data Configuration = Configuration
    { defaultDirectory      :: !FilePath   -- ^ The default directory where figures will be saved.
    , defaultWithSource     :: !Bool       -- ^ The default behavior of whether or not to include links to source code and high-res
    , defaultDPI            :: !Int        -- ^ The default dots-per-inch value for generated figures. Renderers might ignore this.
    , defaultSaveFormat     :: !SaveFormat -- ^ The default save format of generated figures.
    , captionFormat         :: !Format     -- ^ Caption format, in the same notation as Pandoc format, e.g. "markdown+tex_math_dollars"

    , logVerbosity          :: !Verbosity  -- ^ Level of logging verbosity.
    , logSink               :: !LogSink    -- ^ Method of logging, i.e. printing to stderr or file.

    , matplotlibPreamble    :: !Script     -- ^ The default preamble script for the matplotlib toolkit.
    , plotlyPythonPreamble  :: !Script     -- ^ The default preamble script for the Plotly/Python toolkit.
    , plotlyRPreamble       :: !Script     -- ^ The default preamble script for the Plotly/R toolkit.
    , matlabPreamble        :: !Script     -- ^ The default preamble script for the MATLAB toolkit.
    , mathematicaPreamble   :: !Script     -- ^ The default preamble script for the Mathematica toolkit.
    , octavePreamble        :: !Script     -- ^ The default preamble script for the GNU Octave toolkit.
    , ggplot2Preamble       :: !Script     -- ^ The default preamble script for the GGPlot2 toolkit.
    , gnuplotPreamble       :: !Script     -- ^ The default preamble script for the gnuplot toolkit.
    , graphvizPreamble      :: !Script     -- ^ The default preamble script for the Graphviz toolkit.
    , bokehPreamble         :: !Script     -- ^ The default preamble script for the Python/Bokeh toolkit.
    , plotsjlPreamble    :: !Script     -- ^ The default preamble script for the Julia/Plots.jl toolkit.
    
    , matplotlibExe         :: !FilePath   -- ^ The executable to use to generate figures using the matplotlib toolkit.
    , matlabExe             :: !FilePath   -- ^ The executable to use to generate figures using the MATLAB toolkit.
    , plotlyPythonExe       :: !FilePath   -- ^ The executable to use to generate figures using the Plotly/Python toolkit.
    , plotlyRExe            :: !FilePath   -- ^ The executable to use to generate figures using the Plotly/R toolkit.
    , mathematicaExe        :: !FilePath   -- ^ The executable to use to generate figures using the Mathematica toolkit.
    , octaveExe             :: !FilePath   -- ^ The executable to use to generate figures using the GNU Octave toolkit.
    , ggplot2Exe            :: !FilePath   -- ^ The executable to use to generate figures using the GGPlot2 toolkit.
    , gnuplotExe            :: !FilePath   -- ^ The executable to use to generate figures using the gnuplot toolkit.
    , graphvizExe           :: !FilePath   -- ^ The executable to use to generate figures using the Graphviz toolkit.
    , bokehExe              :: !FilePath   -- ^ The executable to use to generate figures using the Python/Bokeh toolkit.
    , plotsjlExe         :: !FilePath   -- ^ The executable to use to generate figures using the Julia/Plots.jl toolkit.
    
    , matplotlibTightBBox   :: !Bool       -- ^ Whether or not to make Matplotlib figures tight by default.
    , matplotlibTransparent :: !Bool       -- ^ Whether or not to make Matplotlib figures transparent by default.
    } deriving (Eq, Show)