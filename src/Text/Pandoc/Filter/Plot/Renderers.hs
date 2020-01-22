{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Specification of renderers.
-}

module Text.Pandoc.Filter.Plot.Renderers (
      scriptExtension
    , comment
    , preambleSelector
    , supportedSaveFormats
    , scriptChecks
    , parseExtraAttrs
    , command
    , capture
    , availableToolkits
    , unavailableToolkits
) where

import           Control.Concurrent.ParallelIO.Local

import           Data.List                                     ((\\))
import           Data.Map.Strict                               (Map)
import           Data.Maybe                                    (catMaybes)
import           Data.Text                                     (Text)

import           Text.Pandoc.Filter.Plot.Renderers.Mathematica
import           Text.Pandoc.Filter.Plot.Renderers.Matlab
import           Text.Pandoc.Filter.Plot.Renderers.Matplotlib
import           Text.Pandoc.Filter.Plot.Renderers.Octave
import           Text.Pandoc.Filter.Plot.Renderers.Plotly
import           Text.Pandoc.Filter.Plot.Renderers.GGPlot2

import           Text.Pandoc.Filter.Plot.Types


-- Extension for script files, e.g. ".py", or ".m".
scriptExtension :: Toolkit -> String
scriptExtension Matplotlib   = ".py"
scriptExtension PlotlyPython = ".py"
scriptExtension Matlab       = ".m"
scriptExtension Mathematica  = ".m"
scriptExtension Octave       = ".m"
scriptExtension GGPlot2      = ".r"


-- Make a string into a comment
comment :: Toolkit -> (Text -> Text)
comment Matplotlib   = mappend "# "
comment PlotlyPython = mappend "# "
comment Matlab       = mappend "% "
comment Mathematica  = \t -> mconcat ["(*", t, "*)"]
comment Octave       = mappend "% "
comment GGPlot2      = mappend "# "


-- | The function that maps from configuration to the preamble.
preambleSelector :: Toolkit -> (Configuration -> Script)
preambleSelector Matplotlib   = matplotlibPreamble
preambleSelector PlotlyPython = plotlyPythonPreamble
preambleSelector Matlab       = matlabPreamble
preambleSelector Mathematica  = mathematicaPreamble
preambleSelector Octave       = octavePreamble
preambleSelector GGPlot2      = ggplot2Preamble


-- | Save formats supported by this renderer.
supportedSaveFormats :: Toolkit -> [SaveFormat]
supportedSaveFormats Matplotlib   = matplotlibSupportedSaveFormats
supportedSaveFormats PlotlyPython = plotlyPythonSupportedSaveFormats
supportedSaveFormats Matlab       = matlabSupportedSaveFormats
supportedSaveFormats Mathematica  = mathematicaSupportedSaveFormats
supportedSaveFormats Octave       = octaveSupportedSaveFormats
supportedSaveFormats GGPlot2      = ggplot2SupportedSaveFormats


-- Checks to perform before running a script. If ANY check fails,
-- the figure is not rendered. This is to prevent, for example,
-- blocking operations to occur.
scriptChecks :: Toolkit -> [Script -> CheckResult]
scriptChecks = const mempty


-- | Parse code block headers for extra attributes that are specific
-- to this renderer. By default, no extra attributes are parsed.
parseExtraAttrs :: Toolkit -> Map Text Text -> Map Text Text
parseExtraAttrs Matplotlib = matplotlibExtraAttrs
parseExtraAttrs _          = return mempty


-- | Generate the appropriate command-line command to generate a figure.
command :: Toolkit -> (Configuration -> FigureSpec -> FilePath -> Text)
command Matplotlib   = matplotlibCommand
command PlotlyPython = plotlyPythonCommand
command Matlab       = matlabCommand
command Mathematica  = mathematicaCommand
command Octave       = octaveCommand
command GGPlot2      = ggplot2Command


-- | Script fragment required to capture a figure.
capture :: Toolkit -> (FigureSpec -> FilePath -> Script)
capture Matplotlib   = matplotlibCapture
capture PlotlyPython = plotlyPythonCapture
capture Matlab       = matlabCapture
capture Mathematica  = mathematicaCapture
capture Octave       = octaveCapture
capture GGPlot2      = ggplot2Capture


-- | Check if a toolkit is available, based on the current configuration
toolkitAvailable :: Toolkit -> Configuration -> IO Bool
toolkitAvailable Matplotlib   = matplotlibAvailable
toolkitAvailable PlotlyPython = plotlyPythonAvailable
toolkitAvailable Matlab       = matlabAvailable
toolkitAvailable Mathematica  = mathematicaAvailable
toolkitAvailable Octave       = octaveAvailable
toolkitAvailable GGPlot2      = ggplot2Available


-- | List of toolkits available on this machine.
-- The executables to look for are taken from the configuration.
availableToolkits :: Configuration -> IO [Toolkit]
availableToolkits conf = do
    -- Certain toolkits (e.g. Python-based toolkits)
    -- may take a long time to startup.
    -- Therefore, we check for available of toolkits in parallel.
    -- TODO: benchmark. Is this overkill?
    maybeToolkits <- withPool (length toolkits) $ 
        \pool -> parallel pool (maybeToolkit <$> toolkits)
    return $ catMaybes maybeToolkits
    where
        maybeToolkit tk = do
            available <- toolkitAvailable tk conf
            if available
                then return $ Just tk
                else return Nothing

    
-- | List of toolkits not available on this machine.
-- The executables to look for are taken from the configuration.
unavailableToolkits :: Configuration -> IO [Toolkit]
unavailableToolkits conf = ((\\) toolkits) <$> availableToolkits conf
