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
-- Reading configuration from file
module Text.Pandoc.Filter.Plot.Configuration
  ( configuration,
    configurationPathMeta,
    defaultConfiguration,
  )
where

import Data.Aeson (FromJSON (parseJSON), Key, Value (Null, Object), (.!=), (.:?))
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
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
      sagemathPreamble = mempty,
      d2Preamble = mempty,
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
      sagemathExe = "sage",
      d2Exe = "d2",
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
      sagemathCmdArgs = mempty,
      d2CmdArgs = mempty,
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
-- ---
-- title: My document
-- author: John Doe
-- plot-configuration: /path/to/file.yml
-- ---
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
    _plantumlPrec :: !PlantUMLPrecursor,
    _sagemathPrec :: !SageMathPrecursor,
    _d2Prec :: !D2Precursor
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
      _plantumlPrec = PlantUMLPrecursor Nothing (plantumlExe defaultConfiguration) (plantumlCmdArgs defaultConfiguration),
      _sagemathPrec = SageMathPrecursor Nothing (sagemathExe defaultConfiguration) (sagemathCmdArgs defaultConfiguration),
      _d2Prec = D2Precursor Nothing (d2Exe defaultConfiguration) (d2CmdArgs defaultConfiguration)
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

data SageMathPrecursor = SageMathPrecursor {_sagemathPreamble :: !(Maybe FilePath), _sagemathExe :: !FilePath, _sagemathCmdArgs :: !Text}

data D2Precursor = D2Precursor {_d2Preamble :: !(Maybe FilePath), _d2Exe :: !FilePath, _d2CmdArgs :: !Text}

instance FromJSON LoggingPrecursor where
  parseJSON (Object v) =
    LoggingPrecursor
      <$> v .:? "verbosity" .!= logVerbosity defaultConfiguration
      <*> v .:? "filepath"
  parseJSON _ = fail $ mconcat ["Could not parse logging configuration. "]

asKey :: InclusionKey -> Key
asKey = fromString . show

instance FromJSON MatplotlibPrecursor where
  parseJSON (Object v) =
    MatplotlibPrecursor
      <$> v .:? asKey PreambleK
      <*> v .:? asKey MatplotlibTightBBoxK .!= matplotlibTightBBox defaultConfiguration
      <*> v .:? asKey MatplotlibTransparentK .!= matplotlibTransparent defaultConfiguration
      <*> v .:? asKey ExecutableK .!= matplotlibExe defaultConfiguration
      <*> v .:? asKey CommandLineArgsK .!= matplotlibCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Matplotlib, " configuration."]

instance FromJSON MatlabPrecursor where
  parseJSON (Object v) = MatlabPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= matlabExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= matlabCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Matlab, " configuration."]

instance FromJSON PlotlyPythonPrecursor where
  parseJSON (Object v) = PlotlyPythonPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= plotlyPythonExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= plotlyPythonCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show PlotlyPython, " configuration."]

instance FromJSON PlotlyRPrecursor where
  parseJSON (Object v) = PlotlyRPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= plotlyRExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= plotlyRCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show PlotlyR, " configuration."]

instance FromJSON MathematicaPrecursor where
  parseJSON (Object v) = MathematicaPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= mathematicaExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= mathematicaCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Mathematica, " configuration."]

instance FromJSON OctavePrecursor where
  parseJSON (Object v) = OctavePrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= octaveExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= octaveCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Octave, " configuration."]

instance FromJSON GGPlot2Precursor where
  parseJSON (Object v) = GGPlot2Precursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= ggplot2Exe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= ggplot2CmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show GGPlot2, " configuration."]

instance FromJSON GNUPlotPrecursor where
  parseJSON (Object v) = GNUPlotPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= gnuplotExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= gnuplotCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show GNUPlot, " configuration."]

instance FromJSON GraphvizPrecursor where
  parseJSON (Object v) = GraphvizPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= graphvizExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= graphvizCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Graphviz, " configuration."]

instance FromJSON BokehPrecursor where
  parseJSON (Object v) = BokehPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= bokehExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= bokehCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Bokeh, " configuration."]

instance FromJSON PlotsjlPrecursor where
  parseJSON (Object v) = PlotsjlPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= plotsjlExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= plotsjlCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show Plotsjl, " configuration."]

instance FromJSON PlantUMLPrecursor where
  parseJSON (Object v) = PlantUMLPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= plantumlExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= plantumlCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show PlantUML, " configuration."]

instance FromJSON SageMathPrecursor where
  parseJSON (Object v) = SageMathPrecursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= sagemathExe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= sagemathCmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show SageMath, " configuration."]

instance FromJSON D2Precursor where
  parseJSON (Object v) = D2Precursor <$> v .:? asKey PreambleK <*> v .:? asKey ExecutableK .!= d2Exe defaultConfiguration <*> v .:? asKey CommandLineArgsK .!= d2CmdArgs defaultConfiguration
  parseJSON _ = fail $ mconcat ["Could not parse ", show SageMath, " configuration."]

toolkitAsKey :: Toolkit -> Key
toolkitAsKey = fromString . unpack . cls

instance FromJSON ConfigPrecursor where
  parseJSON Null = return defaultConfigPrecursor -- In case of empty file
  parseJSON (Object v) = do
    _defaultDirectory <- v .:? asKey DirectoryK .!= _defaultDirectory defaultConfigPrecursor
    _defaultWithSource <- v .:? asKey WithSourceK .!= _defaultWithSource defaultConfigPrecursor
    _defaultDPI <- v .:? asKey DpiK .!= _defaultDPI defaultConfigPrecursor
    _defaultSaveFormat <- v .:? asKey SaveFormatK .!= _defaultSaveFormat defaultConfigPrecursor
    _defaultDependencies <- v .:? asKey DependenciesK .!= _defaultDependencies defaultConfigPrecursor
    _captionFormat <- v .:? asKey CaptionFormatK .!= _captionFormat defaultConfigPrecursor
    _sourceCodeLabel <- v .:? asKey SourceCodeLabelK .!= _sourceCodeLabel defaultConfigPrecursor
    _strictMode <- v .:? asKey StrictModeK .!= _strictMode defaultConfigPrecursor
    _logPrec <- v .:? "logging" .!= _logPrec defaultConfigPrecursor

    _matplotlibPrec <- v .:? toolkitAsKey Matplotlib .!= _matplotlibPrec defaultConfigPrecursor
    _matlabPrec <- v .:? toolkitAsKey Matlab .!= _matlabPrec defaultConfigPrecursor
    _plotlyPythonPrec <- v .:? toolkitAsKey PlotlyPython .!= _plotlyPythonPrec defaultConfigPrecursor
    _plotlyRPrec <- v .:? toolkitAsKey PlotlyR .!= _plotlyRPrec defaultConfigPrecursor
    _mathematicaPrec <- v .:? toolkitAsKey Mathematica .!= _mathematicaPrec defaultConfigPrecursor
    _octavePrec <- v .:? toolkitAsKey Octave .!= _octavePrec defaultConfigPrecursor
    _ggplot2Prec <- v .:? toolkitAsKey GGPlot2 .!= _ggplot2Prec defaultConfigPrecursor
    _gnuplotPrec <- v .:? toolkitAsKey GNUPlot .!= _gnuplotPrec defaultConfigPrecursor
    _graphvizPrec <- v .:? toolkitAsKey Graphviz .!= _graphvizPrec defaultConfigPrecursor
    _bokehPrec <- v .:? toolkitAsKey Bokeh .!= _bokehPrec defaultConfigPrecursor
    _plotsjlPrec <- v .:? toolkitAsKey Plotsjl .!= _plotsjlPrec defaultConfigPrecursor
    _plantumlPrec <- v .:? toolkitAsKey PlantUML .!= _plantumlPrec defaultConfigPrecursor
    _sagemathPrec <- v .:? toolkitAsKey SageMath .!= _sagemathPrec defaultConfigPrecursor
    _d2Prec <- v .:? toolkitAsKey D2 .!= _d2Prec defaultConfigPrecursor

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
      sagemathExe = _sagemathExe _sagemathPrec
      d2Exe = _d2Exe _d2Prec

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
      sagemathCmdArgs = _sagemathCmdArgs _sagemathPrec
      d2CmdArgs = _d2CmdArgs _d2Prec

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
  sagemathPreamble <- readPreamble (_sagemathPreamble _sagemathPrec)
  d2Preamble <- readPreamble (_d2Preamble _d2Prec)

  return Configuration {..}
  where
    readPreamble = maybe mempty TIO.readFile
