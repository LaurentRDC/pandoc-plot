{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Reading configuration from file
module Text.Pandoc.Filter.Plot.Configuration
  ( configuration,
    configurationPathMeta,
    defaultConfiguration,
  )
where

import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Data.Yaml (FromJSON (parseJSON), Value (Null, Object), (.!=), (.:?))
import Data.Yaml.Config (ignoreEnv, loadYamlSettings)
import System.FilePath (normalise)
import Text.Pandoc.Definition (Format (..), Inline (..), MetaValue (..), Pandoc (..), lookupMeta)
import Text.Pandoc.Filter.Plot.Monad

-- | Read configuration from a YAML file. The
-- keys are exactly the same as for code blocks.
--
-- If a key is not present, its value will be set
-- to the default value. Parsing errors result in thrown exceptions.
configuration :: FilePath -> IO Configuration
configuration fp = loadYamlSettings [normalise fp] [] ignoreEnv >>= renderConfig

-- | Default configuration values.
--
-- @since 0.5.0.0
defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { defaultDirectory = "plots/",
      defaultWithSource = False,
      defaultDPI = 80,
      defaultSaveFormat = PNG,
      defaultDependencies = mempty,
      captionFormat = Format "markdown+tex_math_dollars",
      sourceCodeLabel = "Source code",
      strictMode = False,
      logVerbosity = Warning,
      logSink = StdErr,
      -- Preambles
      matplotlibPreamble = mempty,
      plotlyPythonPreamble = mempty,
      plotlyRPreamble = mempty,
      matlabPreamble = mempty,
      mathematicaPreamble = mempty,
      octavePreamble = mempty,
      ggplot2Preamble = mempty,
      gnuplotPreamble = mempty,
      graphvizPreamble = mempty,
      bokehPreamble = mempty,
      plotsjlPreamble = mempty,
      plantumlPreamble = mempty,
      -- Executables
      matplotlibExe = python,
      matlabExe = "matlab",
      plotlyPythonExe = python,
      plotlyRExe = "Rscript",
      mathematicaExe = "math",
      octaveExe = "octave",
      ggplot2Exe = "Rscript",
      gnuplotExe = "gnuplot",
      graphvizExe = "dot",
      bokehExe = python,
      plotsjlExe = "julia",
      plantumlExe = "java",
      -- Command line arguments
      matplotlibCmdArgs = mempty,
      matlabCmdArgs = mempty,
      plotlyPythonCmdArgs = mempty,
      plotlyRCmdArgs = mempty,
      mathematicaCmdArgs = mempty,
      octaveCmdArgs = mempty,
      ggplot2CmdArgs = mempty,
      gnuplotCmdArgs = mempty,
      graphvizCmdArgs = mempty,
      bokehCmdArgs = mempty,
      plotsjlCmdArgs = mempty,
      plantumlCmdArgs = "-jar plantuml.jar",
      -- Extras
      matplotlibTightBBox = False,
      matplotlibTransparent = False
    }
  where
    python = if isWindows then "python" else "python3"

-- | Extact path to configuration from the metadata in a Pandoc document.
-- The path to the configuration file should be under the @plot-configuration@ key.
-- In case there is no such metadata, return the default configuration.
--
-- For example, at the top of a markdown file:
--
-- @
--     ---
--     title: My document
--     author: John Doe
--     plot-configuration: /path/to/file.yml
--     ---
-- @
--
-- The same can be specified via the command line using Pandoc's @-M@ flag:
--
-- > pandoc --filter pandoc-plot -M plot-configuration="path/to/file.yml" ...
--
-- @since 0.6.0.0
configurationPathMeta :: Pandoc -> Maybe FilePath
configurationPathMeta (Pandoc meta _) =
  lookupMeta "plot-configuration" meta >>= getPath
  where
    getPath (MetaString t) = Just (unpack t)
    getPath (MetaInlines [Str s]) = Just (unpack s)
    getPath _ = Nothing

-- We define a precursor type because preambles are best specified as file paths,
-- but we want to read those files before building a full
-- @Configuration@ value.
data ConfigPrecursor = ConfigPrecursor
  { _defaultDirectory :: !FilePath,
    _defaultWithSource :: !Bool,
    _defaultDPI :: !Int,
    _defaultSaveFormat :: !SaveFormat,
    _defaultDependencies :: ![FilePath],
    _captionFormat :: !Format,
    _sourceCodeLabel :: !Text,
    _strictMode :: !Bool,
    _logPrec :: !LoggingPrecursor,
    _matplotlibPrec :: !MatplotlibPrecursor,
    _matlabPrec :: !MatlabPrecursor,
    _plotlyPythonPrec :: !PlotlyPythonPrecursor,
    _plotlyRPrec :: !PlotlyRPrecursor,
    _mathematicaPrec :: !MathematicaPrecursor,
    _octavePrec :: !OctavePrecursor,
    _ggplot2Prec :: !GGPlot2Precursor,
    _gnuplotPrec :: !GNUPlotPrecursor,
    _graphvizPrec :: !GraphvizPrecursor,
    _bokehPrec :: !BokehPrecursor,
    _plotsjlPrec :: !PlotsjlPrecursor,
    _plantumlPrec :: !PlantUMLPrecursor
  }

defaultConfigPrecursor :: ConfigPrecursor
defaultConfigPrecursor =
  ConfigPrecursor
    { _defaultDirectory = defaultDirectory defaultConfiguration,
      _defaultWithSource = defaultWithSource defaultConfiguration,
      _defaultDPI = defaultDPI defaultConfiguration,
      _defaultSaveFormat = defaultSaveFormat defaultConfiguration,
      _defaultDependencies = defaultDependencies defaultConfiguration,
      _captionFormat = captionFormat defaultConfiguration,
      _sourceCodeLabel = sourceCodeLabel defaultConfiguration,
      _strictMode = strictMode defaultConfiguration,
      _logPrec = LoggingPrecursor (logVerbosity defaultConfiguration) Nothing, -- _logFilePath=Nothing implies log to stderr
      _matplotlibPrec = MatplotlibPrecursor Nothing (matplotlibTightBBox defaultConfiguration) (matplotlibTransparent defaultConfiguration) (matplotlibExe defaultConfiguration) (matplotlibCmdArgs defaultConfiguration),
      _matlabPrec = MatlabPrecursor Nothing (matlabExe defaultConfiguration) (matlabCmdArgs defaultConfiguration),
      _plotlyPythonPrec = PlotlyPythonPrecursor Nothing (plotlyPythonExe defaultConfiguration) (plotlyPythonCmdArgs defaultConfiguration),
      _plotlyRPrec = PlotlyRPrecursor Nothing (plotlyRExe defaultConfiguration) (plotlyRCmdArgs defaultConfiguration),
      _mathematicaPrec = MathematicaPrecursor Nothing (mathematicaExe defaultConfiguration) (mathematicaCmdArgs defaultConfiguration),
      _octavePrec = OctavePrecursor Nothing (octaveExe defaultConfiguration) (octaveCmdArgs defaultConfiguration),
      _ggplot2Prec = GGPlot2Precursor Nothing (ggplot2Exe defaultConfiguration) (ggplot2CmdArgs defaultConfiguration),
      _gnuplotPrec = GNUPlotPrecursor Nothing (gnuplotExe defaultConfiguration) (gnuplotCmdArgs defaultConfiguration),
      _graphvizPrec = GraphvizPrecursor Nothing (graphvizExe defaultConfiguration) (graphvizCmdArgs defaultConfiguration),
      _bokehPrec = BokehPrecursor Nothing (bokehExe defaultConfiguration) (bokehCmdArgs defaultConfiguration),
      _plotsjlPrec = PlotsjlPrecursor Nothing (plotsjlExe defaultConfiguration) (plotsjlCmdArgs defaultConfiguration),
      _plantumlPrec = PlantUMLPrecursor Nothing (plantumlExe defaultConfiguration) (plantumlCmdArgs defaultConfiguration)
    }

data LoggingPrecursor = LoggingPrecursor
  { _logVerbosity :: !Verbosity,
    _logFilePath :: !(Maybe FilePath)
  }

-- Separate YAML clauses have their own types.
data MatplotlibPrecursor = MatplotlibPrecursor
  { _matplotlibPreamble :: !(Maybe FilePath),
    _matplotlibTightBBox :: !Bool,
    _matplotlibTransparent :: !Bool,
    _matplotlibExe :: !FilePath,
    _matplotlibCmdArgs :: !Text
  }

data MatlabPrecursor = MatlabPrecursor {_matlabPreamble :: !(Maybe FilePath), _matlabExe :: !FilePath, _matlabCmdArgs :: !Text}

data PlotlyPythonPrecursor = PlotlyPythonPrecursor {_plotlyPythonPreamble :: !(Maybe FilePath), _plotlyPythonExe :: !FilePath, _plotlyPythonCmdArgs :: !Text}

data PlotlyRPrecursor = PlotlyRPrecursor {_plotlyRPreamble :: !(Maybe FilePath), _plotlyRExe :: !FilePath, _plotlyRCmdArgs :: !Text}

data MathematicaPrecursor = MathematicaPrecursor {_mathematicaPreamble :: !(Maybe FilePath), _mathematicaExe :: !FilePath, _mathematicaCmdArgs :: !Text}

data OctavePrecursor = OctavePrecursor {_octavePreamble :: !(Maybe FilePath), _octaveExe :: !FilePath, _octaveCmdArgs :: !Text}

data GGPlot2Precursor = GGPlot2Precursor {_ggplot2Preamble :: !(Maybe FilePath), _ggplot2Exe :: !FilePath, _ggplot2CmdArgs :: !Text}

data GNUPlotPrecursor = GNUPlotPrecursor {_gnuplotPreamble :: !(Maybe FilePath), _gnuplotExe :: !FilePath, _gnuplotCmdArgs :: !Text}

data GraphvizPrecursor = GraphvizPrecursor {_graphvizPreamble :: !(Maybe FilePath), _graphvizExe :: !FilePath, _graphvizCmdArgs :: !Text}

data BokehPrecursor = BokehPrecursor {_bokehPreamble :: !(Maybe FilePath), _bokehExe :: !FilePath, _bokehCmdArgs :: !Text}

data PlotsjlPrecursor = PlotsjlPrecursor {_plotsjlPreamble :: !(Maybe FilePath), _plotsjlExe :: !FilePath, _plotsjlCmdArgs :: !Text}

data PlantUMLPrecursor = PlantUMLPrecursor {_plantumlPreamble :: !(Maybe FilePath), _plantumlExe :: !FilePath, _plantumlCmdArgs :: !Text}

instance FromJSON LoggingPrecursor where
  parseJSON (Object v) =
    LoggingPrecursor <$> v .:? "verbosity" .!= logVerbosity defaultConfiguration
      <*> v .:? "filepath"
  parseJSON _ = fail $ mconcat ["Could not parse logging configuration. "]

instance FromJSON MatplotlibPrecursor where
  parseJSON (Object v) =
    MatplotlibPrecursor
      <$> v .:? tshow PreambleK
      <*> v .:? tshow MatplotlibTightBBoxK .!= matplotlibTightBBox defaultConfiguration
      <*> v .:? tshow MatplotlibTransparentK .!= matplotlibTransparent defaultConfiguration
      <*> v .:? tshow ExecutableK .!= matplotlibExe defaultConfiguration
      <*> v .:? tshow CommandLineArgsK .!= matplotlibCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Matplotlib, " configuration."]

instance FromJSON MatlabPrecursor where
  parseJSON (Object v) = MatlabPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= matlabExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= matlabCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Matlab, " configuration."]

instance FromJSON PlotlyPythonPrecursor where
  parseJSON (Object v) = PlotlyPythonPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= plotlyPythonExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= plotlyPythonCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show PlotlyPython, " configuration."]

instance FromJSON PlotlyRPrecursor where
  parseJSON (Object v) = PlotlyRPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= plotlyRExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= plotlyRCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show PlotlyR, " configuration."]

instance FromJSON MathematicaPrecursor where
  parseJSON (Object v) = MathematicaPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= mathematicaExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= mathematicaCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Mathematica, " configuration."]

instance FromJSON OctavePrecursor where
  parseJSON (Object v) = OctavePrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= octaveExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= octaveCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Octave, " configuration."]

instance FromJSON GGPlot2Precursor where
  parseJSON (Object v) = GGPlot2Precursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= ggplot2Exe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= ggplot2CmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show GGPlot2, " configuration."]

instance FromJSON GNUPlotPrecursor where
  parseJSON (Object v) = GNUPlotPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= gnuplotExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= gnuplotCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show GNUPlot, " configuration."]

instance FromJSON GraphvizPrecursor where
  parseJSON (Object v) = GraphvizPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= graphvizExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= graphvizCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Graphviz, " configuration."]

instance FromJSON BokehPrecursor where
  parseJSON (Object v) = BokehPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= bokehExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= bokehCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Bokeh, " configuration."]

instance FromJSON PlotsjlPrecursor where
  parseJSON (Object v) = PlotsjlPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= plotsjlExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= plotsjlCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Plotsjl, " configuration."]

instance FromJSON PlantUMLPrecursor where
  parseJSON (Object v) = PlantUMLPrecursor <$> v .:? tshow PreambleK <*> v .:? tshow ExecutableK .!= plantumlExe defaultConfiguration <*> v .:? tshow CommandLineArgsK .!= plantumlCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show PlantUML, " configuration."]

instance FromJSON ConfigPrecursor where
  parseJSON Null = return defaultConfigPrecursor -- In case of empty file
  parseJSON (Object v) = do
    _defaultDirectory <- v .:? tshow DirectoryK .!= _defaultDirectory defaultConfigPrecursor
    _defaultWithSource <- v .:? tshow WithSourceK .!= _defaultWithSource defaultConfigPrecursor
    _defaultDPI <- v .:? tshow DpiK .!= _defaultDPI defaultConfigPrecursor
    _defaultSaveFormat <- v .:? tshow SaveFormatK .!= _defaultSaveFormat defaultConfigPrecursor
    _defaultDependencies <- v .:? tshow DependenciesK .!= _defaultDependencies defaultConfigPrecursor
    _captionFormat <- v .:? tshow CaptionFormatK .!= _captionFormat defaultConfigPrecursor
    _sourceCodeLabel <- v .:? tshow SourceCodeLabelK .!= _sourceCodeLabel defaultConfigPrecursor
    _strictMode <- v .:? tshow StrictModeK .!= _strictMode defaultConfigPrecursor
    _logPrec <- v .:? "logging" .!= _logPrec defaultConfigPrecursor

    _matplotlibPrec <- v .:? cls Matplotlib .!= _matplotlibPrec defaultConfigPrecursor
    _matlabPrec <- v .:? cls Matlab .!= _matlabPrec defaultConfigPrecursor
    _plotlyPythonPrec <- v .:? cls PlotlyPython .!= _plotlyPythonPrec defaultConfigPrecursor
    _plotlyRPrec <- v .:? cls PlotlyR .!= _plotlyRPrec defaultConfigPrecursor
    _mathematicaPrec <- v .:? cls Mathematica .!= _mathematicaPrec defaultConfigPrecursor
    _octavePrec <- v .:? cls Octave .!= _octavePrec defaultConfigPrecursor
    _ggplot2Prec <- v .:? cls GGPlot2 .!= _ggplot2Prec defaultConfigPrecursor
    _gnuplotPrec <- v .:? cls GNUPlot .!= _gnuplotPrec defaultConfigPrecursor
    _graphvizPrec <- v .:? cls Graphviz .!= _graphvizPrec defaultConfigPrecursor
    _bokehPrec <- v .:? cls Bokeh .!= _bokehPrec defaultConfigPrecursor
    _plotsjlPrec <- v .:? cls Plotsjl .!= _plotsjlPrec defaultConfigPrecursor
    _plantumlPrec <- v .:? cls PlantUML .!= _plantumlPrec defaultConfigPrecursor

    return $ ConfigPrecursor {..}
  parseJSON _ = fail "Could not parse configuration."

renderConfig :: ConfigPrecursor -> IO Configuration
renderConfig ConfigPrecursor {..} = do
  let defaultDirectory = _defaultDirectory
      defaultWithSource = _defaultWithSource
      defaultDPI = _defaultDPI
      defaultSaveFormat = _defaultSaveFormat
      defaultDependencies = _defaultDependencies
      captionFormat = _captionFormat
      sourceCodeLabel = _sourceCodeLabel
      strictMode = _strictMode

      logVerbosity = _logVerbosity _logPrec
      logSink = maybe StdErr LogFile (_logFilePath _logPrec)

      matplotlibTightBBox = _matplotlibTightBBox _matplotlibPrec
      matplotlibTransparent = _matplotlibTransparent _matplotlibPrec

      matplotlibExe = _matplotlibExe _matplotlibPrec
      matlabExe = _matlabExe _matlabPrec
      plotlyPythonExe = _plotlyPythonExe _plotlyPythonPrec
      plotlyRExe = _plotlyRExe _plotlyRPrec
      mathematicaExe = _mathematicaExe _mathematicaPrec
      octaveExe = _octaveExe _octavePrec
      ggplot2Exe = _ggplot2Exe _ggplot2Prec
      gnuplotExe = _gnuplotExe _gnuplotPrec
      graphvizExe = _graphvizExe _graphvizPrec
      bokehExe = _bokehExe _bokehPrec
      plotsjlExe = _plotsjlExe _plotsjlPrec
      plantumlExe = _plantumlExe _plantumlPrec

      matplotlibCmdArgs = _matplotlibCmdArgs _matplotlibPrec
      matlabCmdArgs = _matlabCmdArgs _matlabPrec
      plotlyPythonCmdArgs = _plotlyPythonCmdArgs _plotlyPythonPrec
      plotlyRCmdArgs = _plotlyRCmdArgs _plotlyRPrec
      mathematicaCmdArgs = _mathematicaCmdArgs _mathematicaPrec
      octaveCmdArgs = _octaveCmdArgs _octavePrec
      ggplot2CmdArgs = _ggplot2CmdArgs _ggplot2Prec
      gnuplotCmdArgs = _gnuplotCmdArgs _gnuplotPrec
      graphvizCmdArgs = _graphvizCmdArgs _graphvizPrec
      bokehCmdArgs = _bokehCmdArgs _bokehPrec
      plotsjlCmdArgs = _plotsjlCmdArgs _plotsjlPrec
      plantumlCmdArgs = _plantumlCmdArgs _plantumlPrec

  matplotlibPreamble <- readPreamble (_matplotlibPreamble _matplotlibPrec)
  matlabPreamble <- readPreamble (_matlabPreamble _matlabPrec)
  plotlyPythonPreamble <- readPreamble (_plotlyPythonPreamble _plotlyPythonPrec)
  plotlyRPreamble <- readPreamble (_plotlyRPreamble _plotlyRPrec)
  mathematicaPreamble <- readPreamble (_mathematicaPreamble _mathematicaPrec)
  octavePreamble <- readPreamble (_octavePreamble _octavePrec)
  ggplot2Preamble <- readPreamble (_ggplot2Preamble _ggplot2Prec)
  gnuplotPreamble <- readPreamble (_gnuplotPreamble _gnuplotPrec)
  graphvizPreamble <- readPreamble (_graphvizPreamble _graphvizPrec)
  bokehPreamble <- readPreamble (_bokehPreamble _bokehPrec)
  plotsjlPreamble <- readPreamble (_plotsjlPreamble _plotsjlPrec)
  plantumlPreamble <- readPreamble (_plantumlPreamble _plantumlPrec)

  return Configuration {..}
  where
    readPreamble fp = maybe mempty TIO.readFile fp

tshow :: Show a => a -> Text
tshow = pack . show
