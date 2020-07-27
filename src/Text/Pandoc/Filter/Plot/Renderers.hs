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
    , executable
    , toolkitAvailable
    , availableToolkits
    , availableToolkitsM
    , unavailableToolkits
    , unavailableToolkitsM
    , OutputSpec(..)
) where

import Control.Concurrent.Async.Lifted               (forConcurrently)

import Data.List                                     ((\\))
import Data.Map.Strict                               (Map)
import Data.Maybe                                    (catMaybes)
import Data.Text                                     (Text)

import Text.Pandoc.Filter.Plot.Renderers.Mathematica
import Text.Pandoc.Filter.Plot.Renderers.Matlab
import Text.Pandoc.Filter.Plot.Renderers.Matplotlib
import Text.Pandoc.Filter.Plot.Renderers.Octave
import Text.Pandoc.Filter.Plot.Renderers.PlotlyPython
import Text.Pandoc.Filter.Plot.Renderers.PlotlyR
import Text.Pandoc.Filter.Plot.Renderers.GGPlot2
import Text.Pandoc.Filter.Plot.Renderers.GNUPlot
import Text.Pandoc.Filter.Plot.Renderers.Graphviz
import Text.Pandoc.Filter.Plot.Renderers.Bokeh
import Text.Pandoc.Filter.Plot.Renderers.Plotsjl
import Text.Pandoc.Filter.Plot.Renderers.Prelude     (executable, OutputSpec(..))

import Text.Pandoc.Filter.Plot.Monad


-- Extension for script files, e.g. ".py", or ".m".
scriptExtension :: Toolkit -> String
scriptExtension Matplotlib   = ".py"
scriptExtension PlotlyPython = ".py"
scriptExtension PlotlyR      = ".r"
scriptExtension Matlab       = ".m"
scriptExtension Mathematica  = ".m"
scriptExtension Octave       = ".m"
scriptExtension GGPlot2      = ".r"
scriptExtension GNUPlot      = ".gp"
scriptExtension Graphviz     = ".dot"
scriptExtension Bokeh        = ".py"
scriptExtension Plotsjl      = ".jl"


-- Make a string into a comment
comment :: Toolkit -> (Text -> Text)
comment Matplotlib   = mappend "# "
comment PlotlyPython = mappend "# "
comment PlotlyR      = mappend "# "
comment Matlab       = mappend "% "
comment Mathematica  = \t -> mconcat ["(*", t, "*)"]
comment Octave       = mappend "% "
comment GGPlot2      = mappend "# "
comment GNUPlot      = mappend "# "
comment Graphviz     = mappend "// "
comment Bokeh        = mappend "# "
comment Plotsjl      = mappend "# "


-- | The function that maps from configuration to the preamble.
preambleSelector :: Toolkit -> (Configuration -> Script)
preambleSelector Matplotlib   = matplotlibPreamble
preambleSelector PlotlyPython = plotlyPythonPreamble
preambleSelector PlotlyR      = plotlyRPreamble
preambleSelector Matlab       = matlabPreamble
preambleSelector Mathematica  = mathematicaPreamble
preambleSelector Octave       = octavePreamble
preambleSelector GGPlot2      = ggplot2Preamble
preambleSelector GNUPlot      = gnuplotPreamble
preambleSelector Graphviz     = graphvizPreamble
preambleSelector Bokeh        = bokehPreamble
preambleSelector Plotsjl      = plotsjlPreamble


-- | Save formats supported by this renderer.
supportedSaveFormats :: Toolkit -> [SaveFormat]
supportedSaveFormats Matplotlib   = matplotlibSupportedSaveFormats
supportedSaveFormats PlotlyPython = plotlyPythonSupportedSaveFormats
supportedSaveFormats PlotlyR      = plotlyRSupportedSaveFormats
supportedSaveFormats Matlab       = matlabSupportedSaveFormats
supportedSaveFormats Mathematica  = mathematicaSupportedSaveFormats
supportedSaveFormats Octave       = octaveSupportedSaveFormats
supportedSaveFormats GGPlot2      = ggplot2SupportedSaveFormats
supportedSaveFormats GNUPlot      = gnuplotSupportedSaveFormats
supportedSaveFormats Graphviz     = graphvizSupportedSaveFormats
supportedSaveFormats Bokeh        = bokehSupportedSaveFormats
supportedSaveFormats Plotsjl      = plotsjlSupportedSaveFormats


-- Checks to perform before running a script. If ANY check fails,
-- the figure is not rendered. This is to prevent, for example,
-- blocking operations to occur.
scriptChecks :: Toolkit -> [Script -> CheckResult]
scriptChecks Matplotlib = [matplotlibCheckIfShow]
scriptChecks Bokeh      = [bokehCheckIfShow]
scriptChecks _ = mempty


-- | Parse code block headers for extra attributes that are specific
-- to this renderer. By default, no extra attributes are parsed.
parseExtraAttrs :: Toolkit -> Map Text Text -> Map Text Text
parseExtraAttrs Matplotlib = matplotlibExtraAttrs
parseExtraAttrs _          = return mempty


-- | Generate the appropriate command-line command to generate a figure.
-- The executable will need to be found first, hence the IO monad.
command :: Toolkit 
        -> OutputSpec
        -> PlotM Text
command Matplotlib   = matplotlibCommand
command PlotlyPython = plotlyPythonCommand
command PlotlyR      = plotlyRCommand
command Matlab       = matlabCommand
command Mathematica  = mathematicaCommand
command Octave       = octaveCommand
command GGPlot2      = ggplot2Command
command GNUPlot      = gnuplotCommand
command Graphviz     = graphvizCommand
command Bokeh        = bokehCommand
command Plotsjl      = plotsjlCommand


-- | Script fragment required to capture a figure.
capture :: Toolkit -> (FigureSpec -> FilePath -> Script)
capture Matplotlib   = matplotlibCapture
capture PlotlyPython = plotlyPythonCapture
capture PlotlyR      = plotlyRCapture
capture Matlab       = matlabCapture
capture Mathematica  = mathematicaCapture
capture Octave       = octaveCapture
capture GGPlot2      = ggplot2Capture
capture GNUPlot      = gnuplotCapture
capture Graphviz     = graphvizCapture 
capture Bokeh        = bokehCapture
capture Plotsjl      = plotsjlCapture


-- | Check if a toolkit is available, based on the current configuration
toolkitAvailable :: Toolkit -> PlotM Bool
toolkitAvailable Matplotlib   = matplotlibAvailable
toolkitAvailable PlotlyPython = plotlyPythonAvailable
toolkitAvailable PlotlyR      = plotlyRAvailable
toolkitAvailable Matlab       = matlabAvailable
toolkitAvailable Mathematica  = mathematicaAvailable
toolkitAvailable Octave       = octaveAvailable
toolkitAvailable GGPlot2      = ggplot2Available
toolkitAvailable GNUPlot      = gnuplotAvailable
toolkitAvailable Graphviz     = graphvizAvailable
toolkitAvailable Bokeh        = bokehAvailable
toolkitAvailable Plotsjl      = plotsjlAvailable


-- | List of toolkits available on this machine.
-- The executables to look for are taken from the configuration.
availableToolkits :: Configuration -> IO [Toolkit]
availableToolkits conf = runPlotM conf availableToolkitsM


-- | List of toolkits not available on this machine.
-- The executables to look for are taken from the configur
unavailableToolkits :: Configuration -> IO [Toolkit]
unavailableToolkits conf = runPlotM conf unavailableToolkitsM


-- | Monadic version of @availableToolkits@.
--
-- Note that logging is disabled
availableToolkitsM :: PlotM [Toolkit]
availableToolkitsM = silence $ do
    mtks <- forConcurrently toolkits $  \tk -> do
        available <- toolkitAvailable tk
        if available
            then return $ Just tk
            else return Nothing
    return $ catMaybes mtks

    
-- | Monadic version of @unavailableToolkits@
unavailableToolkitsM :: PlotM [Toolkit]
unavailableToolkitsM = (\\) toolkits <$> availableToolkitsM
