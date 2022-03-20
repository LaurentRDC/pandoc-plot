{-# LANGUAGE DeriveGeneric #-}
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
-- This module defines base types in use in pandoc-plot
module Text.Pandoc.Filter.Plot.Monad.Types
  ( Toolkit (..),
    Renderer (..),
    Script,
    CheckResult (..),
    InclusionKey (..),
    FigureSpec (..),
    OutputSpec (..),
    SaveFormat (..),
    cls,
    extension,
    toolkits,
    inclusionKeys,
    Executable (..),
    exeFromPath,
    -- Utilities
    isWindows,
  )
where

import Data.Char (toLower)
import Data.List (intersperse)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON(..), ToJSON (toJSON), withText)
import GHC.Generics (Generic)
import System.FilePath (splitFileName)
import System.Info (os)
import Text.Pandoc.Definition (Attr)

-- | List of supported toolkits.
toolkits :: [Toolkit]
toolkits = enumFromTo minBound maxBound

-- | Enumeration of supported toolkits
data Toolkit
  = Matplotlib
  | Matlab
  | PlotlyPython
  | PlotlyR
  | Mathematica
  | Octave
  | GGPlot2
  | GNUPlot
  | Graphviz
  | Bokeh
  | Plotsjl
  | PlantUML
  | SageMath
  deriving (Bounded, Eq, Enum, Generic, Ord)

-- | This instance should only be used to display toolkit names
instance Show Toolkit where
  show Matplotlib = "Python/Matplotlib"
  show Matlab = "MATLAB"
  show PlotlyPython = "Python/Plotly"
  show PlotlyR = "R/Plotly"
  show Mathematica = "Mathematica"
  show Octave = "GNU Octave"
  show GGPlot2 = "ggplot2"
  show GNUPlot = "gnuplot"
  show Graphviz = "graphviz"
  show Bokeh = "Python/Bokeh"
  show Plotsjl = "Julia/Plots.jl"
  show PlantUML = "PlantUML"
  show SageMath = "SageMath"

-- | Class name which will trigger the filter
cls :: Toolkit -> Text
cls Matplotlib = "matplotlib"
cls Matlab = "matlabplot"
cls PlotlyPython = "plotly_python"
cls PlotlyR = "plotly_r"
cls Mathematica = "mathplot"
cls Octave = "octaveplot"
cls GGPlot2 = "ggplot2"
cls GNUPlot = "gnuplot"
cls Graphviz = "graphviz"
cls Bokeh = "bokeh"
cls Plotsjl = "plotsjl"
cls PlantUML = "plantuml"
cls SageMath = "sageplot"

-- | Executable program and directory where it can be found.
data Executable = Executable FilePath Text

exeFromPath :: FilePath -> Executable
exeFromPath fp =
  let (dir, name) = splitFileName fp
   in Executable dir (pack name)

-- | Source context for plotting scripts
type Script = Text

-- | Result of checking scripts for problems
data CheckResult
  = CheckPassed
  | CheckFailed Text
  deriving (Eq)

instance Semigroup CheckResult where
  (<>) CheckPassed a = a
  (<>) a CheckPassed = a
  (<>) (CheckFailed msg1) (CheckFailed msg2) = CheckFailed (msg1 <> msg2)

instance Monoid CheckResult where
  mempty = CheckPassed

-- | Description of any possible inclusion key, both in documents
-- and in configuration files.
data InclusionKey
  = DirectoryK
  | CaptionK
  | SaveFormatK
  | WithSourceK
  | CaptionFormatK
  | PreambleK
  | DpiK
  | SourceCodeLabelK
  | StrictModeK
  | ExecutableK
  | CommandLineArgsK
  | DependenciesK
  | FileK
  | MatplotlibTightBBoxK
  | MatplotlibTransparentK
  deriving (Bounded, Eq, Enum)

-- | Keys that pandoc-plot will look for in code blocks.
-- These are only exported for testing purposes.
instance Show InclusionKey where
  show DirectoryK = "directory"
  show CaptionK = "caption"
  show SaveFormatK = "format"
  show WithSourceK = "source"
  show CaptionFormatK = "caption_format"
  show PreambleK = "preamble"
  show DpiK = "dpi"
  show SourceCodeLabelK = "source_label"
  show StrictModeK = "strict"
  show ExecutableK = "executable"
  show CommandLineArgsK = "command_line_arguments"
  show DependenciesK = "dependencies"
  show FileK = "file"
  show MatplotlibTightBBoxK = "tight_bbox"
  show MatplotlibTransparentK = "transparent"

-- | List of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [InclusionKey]
inclusionKeys = enumFromTo (minBound :: InclusionKey) maxBound

-- | Datatype containing all parameters required to specify a figure.
--
-- It is assumed that once a @FigureSpec@ has been created, no configuration
-- can overload it; hence, a @FigureSpec@ completely encodes a particular figure.
data FigureSpec = FigureSpec
  { -- | Renderer to use for this figure.
    renderer_ :: !Renderer,
    -- | Figure caption.
    caption :: !Text,
    -- | Append link to source code in caption.
    withSource :: !Bool,
    -- | Source code for the figure.
    script :: !Script,
    -- | Save format of the figure.
    saveFormat :: !SaveFormat,
    -- | Directory where to save the file.
    directory :: !FilePath,
    -- | Dots-per-inch of figure.
    dpi :: !Int,
    -- | Files/directories on which this figure depends, e.g. data files.
    dependencies :: ![FilePath],
    -- | Renderer-specific extra attributes.
    extraAttrs :: ![(Text, Text)],
    -- | Attributes not related to @pandoc-plot@ will be propagated.
    blockAttrs :: !Attr
  }

-- | Generated figure file format supported by pandoc-plot.
-- Note that not all formats are supported by all toolkits.
data SaveFormat
  = -- | Portable network graphics
    PNG
  | -- | Portable document format
    PDF
  | -- | Scalable vector graphics
    SVG
  | -- | JPEG/JPG compressed image
    JPG
  | -- | Encapsulated postscript
    EPS
  | -- | GIF format
    GIF
  | -- | Tagged image format
    TIF
  | -- | WebP image format
    WEBP
  | -- | HTML for interactive plots.
    HTML
  | -- | LaTeX text and pdf graphics
    LaTeX
  deriving (Bounded, Enum, Ord, Eq, Show, Generic)

instance IsString SaveFormat where
  fromString s
    | s `elem` ["png", "PNG", ".png"] = PNG
    | s `elem` ["pdf", "PDF", ".pdf"] = PDF
    | s `elem` ["svg", "SVG", ".svg"] = SVG
    | s `elem` ["eps", "EPS", ".eps"] = EPS
    | s `elem` ["gif", "GIF", ".gif"] = GIF
    | s `elem` ["jpg", "jpeg", "JPG", "JPEG", ".jpg", ".jpeg"] = JPG
    | s `elem` ["tif", "tiff", "TIF", "TIFF", ".tif", ".tiff"] = TIF
    | s `elem` ["webp", "WEBP", ".webp"] = WEBP
    | s `elem` ["html", "HTML", ".html"] = HTML
    | s `elem` ["latex", "LaTeX", ".tex"] = LaTeX
    | otherwise =
      errorWithoutStackTrace $
        mconcat
          [ s,
            " is not one of the valid save formats : ",
            mconcat $ intersperse ", " $ show <$> saveFormats,
            " (and lowercase variations). "
          ]
    where
      saveFormats = enumFromTo minBound maxBound :: [SaveFormat]

-- | Use the IsString instance to parse JSON so that the parsing is flexible
-- with respect to uppercase/lowercase (#42)
instance FromJSON SaveFormat where
  parseJSON = withText "SaveFormat" (pure . fromString . unpack)

instance ToJSON SaveFormat where
  toJSON = toJSON . extension

-- | Save format file extension
extension :: SaveFormat -> String
extension LaTeX = ".tex"
extension fmt = mconcat [".", fmap toLower . show $ fmt]

isWindows :: Bool
isWindows = os `elem` ["mingw32", "win32", "cygwin32"] -- Aliases taken from cabal's Distribution.System module

-- | Internal description of all information
-- needed to output a figure.
data OutputSpec = OutputSpec
  { -- | Figure spec
    oFigureSpec :: FigureSpec,
    -- | Path to the script to render
    oScriptPath :: FilePath,
    -- | Figure output path
    oFigurePath :: FilePath,
    -- | Current working directory
    oCWD :: FilePath
  }

data Renderer = Renderer
  { rendererToolkit :: Toolkit,
    rendererExe :: Executable,
    rendererCapture :: FigureSpec -> FilePath -> Script,
    rendererCommand :: OutputSpec -> Text,
    rendererSupportedSaveFormats :: [SaveFormat],
    rendererChecks :: [Script -> CheckResult],
    rendererLanguage :: Text,
    rendererComment :: Text -> Text,
    rendererScriptExtension :: String
  }
