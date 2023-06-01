{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.Reader (local)
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text, pack)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import Text.Pandoc.Filter.Plot.Monad
import Text.Pandoc.Filter.Plot.Monad.Logging
  ( Logger (lVerbosity),
  )
import Text.Pandoc.Filter.Plot.Renderers.Bokeh
  ( bokeh,
    bokehSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.D2
  ( d2,
    d2SupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.GGPlot2
  ( ggplot2,
    ggplot2SupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.GNUPlot
  ( gnuplot,
    gnuplotSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.Graphviz
  ( graphviz,
    graphvizSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.Mathematica
  ( mathematica,
    mathematicaSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.Matlab
  ( matlab,
    matlabSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.Matplotlib
  ( matplotlib,
    matplotlibSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.Octave
  ( octave,
    octaveSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.PlantUML
  ( plantuml,
    plantumlSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.PlotlyPython
  ( plotlyPython,
    plotlyPythonSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.PlotlyR
  ( plotlyR,
    plotlyRSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.Plotsjl
  ( plotsjl,
    plotsjlSupportedSaveFormats,
  )
import Text.Pandoc.Filter.Plot.Renderers.SageMath
  ( sagemath,
    sagemathSupportedSaveFormats,
  )

-- | Get the renderer associated with a toolkit.
-- If the renderer has not been used before,
-- initialize it and store where it is. It will be re-used.
renderer :: Toolkit -> PlotM Renderer
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
renderer PlantUML = plantuml
renderer SageMath = sagemath
renderer D2 = d2

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
supportedSaveFormats D2 = d2SupportedSaveFormats

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
preambleSelector D2 = d2Preamble

-- | Parse code block headers for extra attributes that are specific
-- to this renderer. By default, no extra attributes are parsed.
parseExtraAttrs :: Toolkit -> Map Text Text -> Map Text Text
parseExtraAttrs Matplotlib =
  M.filterWithKey
    ( \k _ ->
        k
          `elem` [ pack $ show MatplotlibTightBBoxK,
                   pack $ show MatplotlibTransparentK
                 ]
    )
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
    r <- renderer tk
    exe <- executable tk
    a <- isAvailable exe (rendererAvailability r)
    if a
      then return $ Just tk
      else return Nothing
  return $ catMaybes mtks
  where
    asNonStrictAndSilent = local (\(RuntimeEnv f c l d s) -> RuntimeEnv f (c {strictMode = False}) (l {lVerbosity = Silent}) d s)

    -- \| Check that the supplied command results in
    -- an exit code of 0 (i.e. no errors)
    commandSuccess :: Text -> PlotM Bool
    commandSuccess s = do
      cwd <- asks envCWD
      (ec, _) <- runCommand cwd s
      debug $ mconcat ["Command ", s, " resulted in ", pack $ show ec]
      return $ ec == ExitSuccess

    isAvailable :: Executable -> AvailabilityCheck -> PlotM Bool
    isAvailable exe (CommandSuccess f) = commandSuccess (f exe)
    isAvailable exe ExecutableExists = liftIO $ findExecutable (pathToExe exe) <&> isJust

-- | Monadic version of @unavailableToolkits@
unavailableToolkitsM :: PlotM [Toolkit]
unavailableToolkitsM = (\\) toolkits <$> availableToolkitsM
