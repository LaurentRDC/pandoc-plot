{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2020
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Specification of renderers.
module Text.Pandoc.Filter.Plot.Renderers
  ( renderer,
    preambleSelector,
    parseExtraAttrs,
    executable,
    availableToolkits,
    availableToolkitsM,
    unavailableToolkits,
    unavailableToolkitsM,
    supportedSaveFormats,
    OutputSpec (..),
    Executable (..),
    Renderer (..),
  )
where

import Control.Concurrent.Async.Lifted (forConcurrently)
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Text.Pandoc.Filter.Plot.Monad
import Text.Pandoc.Filter.Plot.Renderers.Bokeh
import Text.Pandoc.Filter.Plot.Renderers.GGPlot2
import Text.Pandoc.Filter.Plot.Renderers.GNUPlot
import Text.Pandoc.Filter.Plot.Renderers.Graphviz
import Text.Pandoc.Filter.Plot.Renderers.Mathematica
import Text.Pandoc.Filter.Plot.Renderers.Matlab
import Text.Pandoc.Filter.Plot.Renderers.Matplotlib
import Text.Pandoc.Filter.Plot.Renderers.Octave
import Text.Pandoc.Filter.Plot.Renderers.PlotlyPython
import Text.Pandoc.Filter.Plot.Renderers.PlotlyR
import Text.Pandoc.Filter.Plot.Renderers.Plotsjl

renderer :: Toolkit -> PlotM (Maybe Renderer)
renderer Matplotlib = matplotlib
renderer PlotlyPython = plotlyPython
renderer PlotlyR = plotlyR
renderer Matlab = matlab
renderer Mathematica = mathematica
renderer Octave = octave
renderer GGPlot2 = ggplot2
renderer GNUPlot = gnuplot
renderer Graphviz = graphviz
renderer Bokeh = bokeh
renderer Plotsjl = plotsjl

-- | Save formats supported by this renderer.
supportedSaveFormats :: Toolkit -> [SaveFormat]
supportedSaveFormats Matplotlib = matplotlibSupportedSaveFormats
supportedSaveFormats PlotlyPython = plotlyPythonSupportedSaveFormats
supportedSaveFormats PlotlyR = plotlyRSupportedSaveFormats
supportedSaveFormats Matlab = matlabSupportedSaveFormats
supportedSaveFormats Mathematica = mathematicaSupportedSaveFormats
supportedSaveFormats Octave = octaveSupportedSaveFormats
supportedSaveFormats GGPlot2 = ggplot2SupportedSaveFormats
supportedSaveFormats GNUPlot = gnuplotSupportedSaveFormats
supportedSaveFormats Graphviz = graphvizSupportedSaveFormats
supportedSaveFormats Bokeh = bokehSupportedSaveFormats
supportedSaveFormats Plotsjl = plotsjlSupportedSaveFormats

-- | The function that maps from configuration to the preamble.
preambleSelector :: Toolkit -> (Configuration -> Script)
preambleSelector Matplotlib = matplotlibPreamble
preambleSelector PlotlyPython = plotlyPythonPreamble
preambleSelector PlotlyR = plotlyRPreamble
preambleSelector Matlab = matlabPreamble
preambleSelector Mathematica = mathematicaPreamble
preambleSelector Octave = octavePreamble
preambleSelector GGPlot2 = ggplot2Preamble
preambleSelector GNUPlot = gnuplotPreamble
preambleSelector Graphviz = graphvizPreamble
preambleSelector Bokeh = bokehPreamble
preambleSelector Plotsjl = plotsjlPreamble

-- | Parse code block headers for extra attributes that are specific
-- to this renderer. By default, no extra attributes are parsed.
parseExtraAttrs :: Toolkit -> Map Text Text -> Map Text Text
parseExtraAttrs Matplotlib = M.filterWithKey (\k _ -> k `elem` ["tight_bbox", "transparent"])
parseExtraAttrs _ = return mempty

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
  mtks <- forConcurrently toolkits $ \tk -> do
    available <- isJust <$> renderer tk
    if available
      then return $ Just tk
      else return Nothing
  return $ catMaybes mtks

-- | Monadic version of @unavailableToolkits@
unavailableToolkitsM :: PlotM [Toolkit]
unavailableToolkitsM = (\\) toolkits <$> availableToolkitsM
