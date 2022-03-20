{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
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
import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Monad.Reader (local)
import Control.Monad.State.Strict
  ( MonadState (get, put),
  )
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text, pack)
import Text.Pandoc.Filter.Plot.Monad
import Text.Pandoc.Filter.Plot.Monad.Logging
  ( Logger (lVerbosity),
  )
import Text.Pandoc.Filter.Plot.Renderers.Bokeh
    ( bokeh, bokehSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.GGPlot2
    ( ggplot2, ggplot2SupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.GNUPlot
    ( gnuplot, gnuplotSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.Graphviz
    ( graphviz, graphvizSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.Mathematica
    ( mathematica, mathematicaSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.Matlab
    ( matlab, matlabSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.Matplotlib
    ( matplotlib, matplotlibSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.Octave
    ( octave, octaveSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.PlantUML
    ( plantuml, plantumlSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.PlotlyPython
    ( plotlyPython, plotlyPythonSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.PlotlyR
    ( plotlyR, plotlyRSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.Plotsjl
    ( plotsjl, plotsjlSupportedSaveFormats )
import Text.Pandoc.Filter.Plot.Renderers.SageMath
    ( sagemath, sagemathSupportedSaveFormats )

-- | Get the renderer associated with a toolkit.
-- If the renderer has not been used before,
-- initialize it and store where it is. It will be re-used.
renderer :: Toolkit -> PlotM (Maybe Renderer)
renderer tk = do
  PlotState varHashes varRenderers <- get
  renderers <- liftIO $ takeMVar varRenderers
  (r', rs') <- case M.lookup tk renderers of
    Nothing -> do
      debug $ mconcat ["Looking for renderer for ", pack $ show tk]
      r' <- sel tk
      let rs' = M.insert tk r' renderers
      return (r', rs')
    Just e -> do
      debug $ mconcat ["Renderer for \"", pack $ show tk, "\" already initialized."]
      return (e, renderers)
  liftIO $ putMVar varRenderers rs'
  put $ PlotState varHashes varRenderers
  return r'
  where
    sel :: Toolkit -> PlotM (Maybe Renderer)
    sel Matplotlib = matplotlib
    sel PlotlyPython = plotlyPython
    sel PlotlyR = plotlyR
    sel Matlab = matlab
    sel Mathematica = mathematica
    sel Octave = octave
    sel GGPlot2 = ggplot2
    sel GNUPlot = gnuplot
    sel Graphviz = graphviz
    sel Bokeh = bokeh
    sel Plotsjl = plotsjl
    sel PlantUML = plantuml
    sel SageMath = sagemath

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
supportedSaveFormats PlantUML = plantumlSupportedSaveFormats
supportedSaveFormats SageMath = sagemathSupportedSaveFormats

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
preambleSelector PlantUML = plantumlPreamble
preambleSelector SageMath = sagemathPreamble

-- | Parse code block headers for extra attributes that are specific
-- to this renderer. By default, no extra attributes are parsed.
parseExtraAttrs :: Toolkit -> Map Text Text -> Map Text Text
parseExtraAttrs Matplotlib = M.filterWithKey (\k _ -> k `elem` ["tight_bbox", "transparent"])
parseExtraAttrs _ = return mempty

-- | List of toolkits available on this machine.
-- The executables to look for are taken from the configuration.
availableToolkits :: Configuration -> IO [Toolkit]
availableToolkits conf = runPlotM Nothing conf availableToolkitsM

-- | List of toolkits not available on this machine.
-- The executables to look for are taken from the configur
unavailableToolkits :: Configuration -> IO [Toolkit]
unavailableToolkits conf = runPlotM Nothing conf unavailableToolkitsM

-- | Monadic version of @availableToolkits@.
--
-- Note that logging is disabled
availableToolkitsM :: PlotM [Toolkit]
availableToolkitsM = asNonStrictAndSilent $ do
    mtks <- forConcurrently toolkits $ \tk -> do
      available <- isJust <$> renderer tk
      if available
        then return $ Just tk
        else return Nothing
    return $ catMaybes mtks
  where
    asNonStrictAndSilent = local (\(RuntimeEnv f c l d) -> RuntimeEnv f (c{strictMode = False}) (l{lVerbosity = Silent}) d)

-- | Monadic version of @unavailableToolkits@
unavailableToolkitsM :: PlotM [Toolkit]
unavailableToolkitsM = (\\) toolkits <$> availableToolkitsM
