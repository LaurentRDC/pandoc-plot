{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines base types in use in pandoc-plot
-}

module Text.Pandoc.Filter.Plot.Monad.Types (
      Toolkit(..)
    , Script
    , CheckResult(..)
    , InclusionKey(..)
    , FigureSpec(..)
    , SaveFormat(..)
    , cls
    , extension
    , toolkits
    , inclusionKeys
    -- Utilities
    , isWindows
) where

import           Data.Char              (toLower)
import           Data.List              (intersperse)
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import           Data.Yaml

import           GHC.Generics           (Generic)
import           System.Info            (os)

import           Text.Pandoc.Definition (Attr)

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
    deriving (Bounded, Eq, Enum, Generic)


-- | This instance should only be used to display toolkit names
instance Show Toolkit where
    show Matplotlib   = "Python/Matplotlib"
    show Matlab       = "MATLAB"
    show PlotlyPython = "Python/Plotly"
    show PlotlyR      = "R/Plotly"
    show Mathematica  = "Mathematica"
    show Octave       = "GNU Octave"
    show GGPlot2      = "ggplot2"
    show GNUPlot      = "gnuplot"
    show Graphviz     = "graphviz"
    show Bokeh        = "Python/Bokeh"
    show Plotsjl      = "Julia/Plots.jl"


-- | Class name which will trigger the filter
cls :: Toolkit -> Text
cls Matplotlib   = "matplotlib"
cls Matlab       = "matlabplot"
cls PlotlyPython = "plotly_python"
cls PlotlyR      = "plotly_r"
cls Mathematica  = "mathplot"
cls Octave       = "octaveplot"
cls GGPlot2      = "ggplot2"
cls GNUPlot      = "gnuplot"
cls Graphviz     = "graphviz"
cls Bokeh        = "bokeh"
cls Plotsjl      = "plotsjl"


-- | Source context for plotting scripts
type Script = Text


-- | Result of checking scripts for problems
data CheckResult
    = CheckPassed
    | CheckFailed Text
    deriving (Eq)

instance Semigroup CheckResult where
    (<>) CheckPassed a                         = a
    (<>) a CheckPassed                         = a
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
    | ExecutableK
    | DependenciesK
    | FileK
    | MatplotlibTightBBoxK
    | MatplotlibTransparentK
    deriving (Bounded, Eq, Enum)

-- | Keys that pandoc-plot will look for in code blocks.
-- These are only exported for testing purposes.
instance Show InclusionKey where
    show DirectoryK             = "directory"
    show CaptionK               = "caption"
    show SaveFormatK            = "format"
    show WithSourceK            = "source"
    show CaptionFormatK         = "caption_format"
    show PreambleK              = "preamble"
    show DpiK                   = "dpi"
    show ExecutableK            = "executable"
    show DependenciesK          = "dependencies"
    show FileK                  = "file"
    show MatplotlibTightBBoxK   = "tight_bbox"
    show MatplotlibTransparentK = "transparent"


-- | List of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [InclusionKey]
inclusionKeys = enumFromTo (minBound::InclusionKey) maxBound


-- | Datatype containing all parameters required to run pandoc-plot.
--
-- It is assumed that once a @FigureSpec@ has been created, no configuration
-- can overload it; hence, a @FigureSpec@ completely encodes a particular figure.
data FigureSpec = FigureSpec
    { toolkit      :: !Toolkit        -- ^ Plotting toolkit to use for this figure.
    , caption      :: !Text           -- ^ Figure caption.
    , withSource   :: !Bool           -- ^ Append link to source code in caption.
    , script       :: !Script         -- ^ Source code for the figure.
    , saveFormat   :: !SaveFormat     -- ^ Save format of the figure.
    , directory    :: !FilePath       -- ^ Directory where to save the file.
    , dpi          :: !Int            -- ^ Dots-per-inch of figure.
    , dependencies :: ![FilePath]     -- ^ Files/directories on which this figure depends, e.g. data files.
    , extraAttrs   :: ![(Text, Text)] -- ^ Renderer-specific extra attributes.
    , blockAttrs   :: !Attr           -- ^ Attributes not related to @pandoc-plot@ will be propagated.
    }


-- | Generated figure file format supported by pandoc-plot.
-- Note that not all formats are supported by all toolkits.
data SaveFormat
    = PNG   -- ^ Portable network graphics
    | PDF   -- ^ Portable document format
    | SVG   -- ^ Scalable vector graphics
    | JPG   -- ^ JPEG/JPG compressed image
    | EPS   -- ^ Encapsulated postscript
    | GIF   -- ^ GIF format
    | TIF   -- ^ Tagged image format
    | WEBP  -- ^ WebP image format
    | HTML  -- ^ HTML for interactive plots.
    deriving (Bounded, Enum, Eq, Show, Generic)

instance IsString SaveFormat where
    -- | An error is thrown if the save format cannot be parsed. That's OK
    -- since pandoc-plot is a command-line tool and isn't expected to run
    -- long.
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
        | otherwise = errorWithoutStackTrace $
                mconcat [ s
                        , " is not one of valid save format : "
                        , mconcat $ intersperse ", " $ show <$> saveFormats
                        ]
        where
            saveFormats =  (enumFromTo minBound maxBound) :: [SaveFormat]

instance FromJSON SaveFormat -- TODO: test this parsing

instance ToJSON SaveFormat where
    toJSON = toJSON . extension


-- | Save format file extension
extension :: SaveFormat -> String
extension fmt = mconcat [".", fmap toLower . show $ fmt]


isWindows :: Bool
isWindows = os `elem` ["mingw32", "win32", "cygwin32"] -- Aliases taken from cabal's Distribution.System module