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

This module defines types in use in pandoc-plot
-}

module Text.Pandoc.Filter.Plot.Types (
      Toolkit(..)
    , PlotM
    , PlotEnv(..)
    , Configuration(..)
    , Script
    , CheckResult(..)
    , InclusionKey(..)
    , FigureSpec(..)
    , SaveFormat(..)
    , figureContentHash
    , cls
    , extension
    , toolkits
    , inclusionKeys
    -- Utilities
    , isWindows
) where

import           Control.Monad.Reader

import           Data.Char              (toLower)
import           Data.Default.Class     (Default, def)
import           Data.Hashable          (hash)
import           Data.List              (intersperse)
import           Data.Semigroup         (Semigroup (..))
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import           Data.Yaml

import           GHC.Generics           (Generic)
import           System.Info            (os)

import           Text.Pandoc.Definition (Attr, Format(..))


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


type PlotM a = ReaderT PlotEnv IO a


data PlotEnv
    = PlotEnv { toolkit   :: !Toolkit
              , config    :: !Configuration
              }

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
--     plot-configuration: /path/to/file.yml
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

    , matplotlibPreamble    :: !Script     -- ^ The default preamble script for the matplotlib toolkit.
    , plotlyPythonPreamble  :: !Script     -- ^ The default preamble script for the Plotly/Python toolkit.
    , plotlyRPreamble       :: !Script     -- ^ The default preamble script for the Plotly/R toolkit.
    , matlabPreamble        :: !Script     -- ^ The default preamble script for the MATLAB toolkit.
    , mathematicaPreamble   :: !Script     -- ^ The default preamble script for the Mathematica toolkit.
    , octavePreamble        :: !Script     -- ^ The default preamble script for the GNU Octave toolkit.
    , ggplot2Preamble       :: !Script     -- ^ The default preamble script for the GGPlot2 toolkit.
    , gnuplotPreamble       :: !Script     -- ^ The default preamble script for the gnuplot toolkit.
    , graphvizPreamble      :: !Script     -- ^ The default preamble script for the Graphviz toolkit.
    
    , matplotlibExe         :: !FilePath   -- ^ The executable to use to generate figures using the matplotlib toolkit.
    , matlabExe             :: !FilePath   -- ^ The executable to use to generate figures using the MATLAB toolkit.
    , plotlyPythonExe       :: !FilePath   -- ^ The executable to use to generate figures using the Plotly/Python toolkit.
    , plotlyRExe            :: !FilePath   -- ^ The executable to use to generate figures using the Plotly/R toolkit.
    , mathematicaExe        :: !FilePath   -- ^ The executable to use to generate figures using the Mathematica toolkit.
    , octaveExe             :: !FilePath   -- ^ The executable to use to generate figures using the GNU Octave toolkit.
    , ggplot2Exe            :: !FilePath   -- ^ The executable to use to generate figures using the GGPlot2 toolkit.
    , gnuplotExe            :: !FilePath   -- ^ The executable to use to generate figures using the gnuplot toolkit.
    , graphvizExe           :: !FilePath   -- ^ The executable to use to generate figures using the Graphviz toolkit.
    
    , matplotlibTightBBox   :: !Bool       -- ^ Whether or not to make Matplotlib figures tight by default.
    , matplotlibTransparent :: !Bool       -- ^ Whether or not to make Matplotlib figures transparent by default.
    } deriving (Eq, Show)

instance Default Configuration where
    def = Configuration
          { defaultDirectory  = "plots/"
          , defaultWithSource = False
          , defaultDPI        = 80
          , defaultSaveFormat = PNG
          , captionFormat     = Format "markdown+tex_math_dollars"
          
          , matplotlibPreamble  = mempty
          , plotlyPythonPreamble= mempty
          , plotlyRPreamble     = mempty
          , matlabPreamble      = mempty
          , mathematicaPreamble = mempty
          , octavePreamble      = mempty
          , ggplot2Preamble     = mempty
          , gnuplotPreamble     = mempty
          , graphvizPreamble    = mempty

          , matplotlibExe       = if isWindows then "python" else "python3"
          , matlabExe           = "matlab"
          , plotlyPythonExe     = if isWindows then "python" else "python3"
          , plotlyRExe          = "Rscript"
          , mathematicaExe      = "math"
          , octaveExe           = "octave"
          , ggplot2Exe          = "Rscript"
          , gnuplotExe          = "gnuplot"
          , graphvizExe         = "dot"
          
          , matplotlibTightBBox   = False
          , matplotlibTransparent = False
          }

type Script = Text


-- | Result of checking scripts for problems
data CheckResult
    = CheckPassed
    | CheckFailed String
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
    { caption    :: !Text           -- ^ Figure caption.
    , withSource :: !Bool           -- ^ Append link to source code in caption.
    , script     :: !Script         -- ^ Source code for the figure.
    , saveFormat :: !SaveFormat     -- ^ Save format of the figure.
    , directory  :: !FilePath       -- ^ Directory where to save the file.
    , dpi        :: !Int            -- ^ Dots-per-inch of figure.
    , extraAttrs :: ![(Text, Text)] -- ^ Renderer-specific extra attributes.
    , blockAttrs :: !Attr           -- ^ Attributes not related to @pandoc-plot@ will be propagated.
    }


-- | Hash of the content of a @FigureSpec@. Note that unlike usual hashes,
-- two @FigureSpec@ with the same @figureContentHash@ does not mean that they are equal!
--
-- Not all parts of a FigureSpec are related to running code.
-- For example, changing the caption should not require running the figure again.
figureContentHash :: FigureSpec -> Int
figureContentHash FigureSpec{..} = 
    hash (script, fromEnum saveFormat, directory, dpi, extraAttrs)


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
        | otherwise = error $
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