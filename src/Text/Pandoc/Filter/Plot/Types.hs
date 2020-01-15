{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}


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
    , cls
    , extension
    , toolkits
    , inclusionKeys
) where

import           Control.Monad.Reader            

import           Data.Char              (toLower)
import           Data.Default.Class     (Default, def)
import           Data.Hashable          (Hashable(..))
import           Data.List              (intersperse)
import           Data.Semigroup         (Semigroup(..))
import           Data.String            (IsString(..))
import           Data.Text              (Text, pack)
import           Data.Yaml

import           GHC.Generics           (Generic)

import           Text.Pandoc.Definition (Attr)

toolkits :: [Toolkit]
toolkits = enumFromTo minBound maxBound

-- | Enumeration of supported toolkits
data Toolkit
    = Matplotlib
    | Matlab
    | PlotlyPython
    | Mathematica
    | Octave
    deriving (Bounded, Eq, Enum, Generic)

-- | This instance should only be used to display toolkit names
instance Show Toolkit where
    show Matplotlib   = "Python/Matplotlib"
    show Matlab       = "MATLAB"
    show PlotlyPython = "Python/Plotly"
    show Mathematica  = "Mathematica"
    show Octave       = "GNU Octave"

-- | Class name which will trigger the filter
cls :: Toolkit -> Text
cls Matplotlib   = "matplotlib"
cls Matlab       = "matlabplot"
cls PlotlyPython = "plotly_python"
cls Mathematica  = "mathplot"
cls Octave       = "octaveplot"


type PlotM a = ReaderT PlotEnv IO a

data PlotEnv 
    = PlotEnv { toolkit :: Toolkit
              , config  :: Configuration
              }

data Configuration = Configuration
    { defaultDirectory      :: FilePath   -- ^ The default directory where figures will be saved.
    , defaultWithSource     :: Bool       -- ^ The default behavior of whether or not to include links to source code and high-res
    , defaultDPI            :: Int        -- ^ The default dots-per-inch value for generated figures. Renderers might ignore this.
    , defaultSaveFormat     :: SaveFormat -- ^ The default save format of generated figures.
    , pythonInterpreter     :: String     -- ^ The default Python interpreter to use for Python-based renderers.
    -- Default preambles
    , matplotlibPreamble    :: Script
    , plotlyPythonPreamble  :: Script
    , matlabPreamble        :: Script
    , mathematicaPreamble   :: Script
    , octavePreamble        :: Script
    -- Toolkit-specific options
    , matplotlibTightBBox   :: Bool
    , matplotlibTransparent :: Bool
    }

instance Default Configuration where
    def = Configuration 
          { defaultDirectory  = "plots/"
          , defaultWithSource = False
          , defaultDPI        = 80
          , defaultSaveFormat = PNG
#if defined(mingw32_HOST_OS)
          , pythonInterpreter = "python"
#else
          , pythonInterpreter = "python3"
#endif
          
          , matplotlibTightBBox   = False
          , matplotlibTransparent = False
          , matplotlibPreamble  = mempty

          , plotlyPythonPreamble= mempty
          
          , matlabPreamble      = mempty

          , mathematicaPreamble = mempty

          , octavePreamble      = mempty
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

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- | Description of any possible inclusion key, both in documents
-- and in configuration files.
data InclusionKey 
    = DirectoryK
    | CaptionK
    | SaveFormatK
    | WithSourceK
    | PreambleK
    | DpiK
    | PyInterpreterK
    | MatplotlibTightBBoxK
    | MatplotlibTransparentK
    | MatplotlibPreambleK
    | PlotlyPreambleK
    | MatlabPreambleK
    | MathematicaPreambleK
    | OctavePreambleK
    deriving (Bounded, Eq, Enum)

-- | Keys that pandoc-plot will look for in code blocks. 
-- These are only exported for testing purposes.
instance Show InclusionKey where
    show DirectoryK      = "directory"
    show CaptionK        = "caption"
    show SaveFormatK     = "format"
    show WithSourceK     = "source"
    show PreambleK       = "preamble"
    show DpiK            = "dpi"
    show PyInterpreterK  = "python_interpreter"
    show MatplotlibTightBBoxK = "tight_bbox"
    show MatplotlibTransparentK = "transparent"
    show MatplotlibPreambleK  = show PreambleK
    show PlotlyPreambleK      = show PreambleK
    show MatlabPreambleK      = show PreambleK
    show MathematicaPreambleK = show PreambleK
    show OctavePreambleK      = show PreambleK


-- | List of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [InclusionKey]
inclusionKeys = enumFromTo (minBound::InclusionKey) maxBound


-- | Datatype containing all parameters required to run pandoc-plot.
--
-- It is assumed that once a @FigureSpec@ has been created, no configuration
-- can overload it; hence, a @FigureSpec@ completely encodes a particular figure.
data FigureSpec = FigureSpec
    { caption    :: Text           -- ^ Figure caption.
    , withSource :: Bool           -- ^ Append link to source code in caption.
    , script     :: Script         -- ^ Source code for the figure.
    , saveFormat :: SaveFormat     -- ^ Save format of the figure.
    , directory  :: FilePath       -- ^ Directory where to save the file.
    , dpi        :: Int            -- ^ Dots-per-inch of figure.
    , extraAttrs :: [(Text, Text)] -- ^ Renderer-specific extra attributes.
    , blockAttrs :: Attr           -- ^ Attributes not related to @pandoc-plot@ will be propagated.
    } deriving Generic

instance Hashable FigureSpec -- From Generic

-- | Generated figure file format supported by pandoc-plot.
-- Note: all formats are supported by Matplotlib, but not all
-- formats are supported by Plotly
data SaveFormat
    = PNG
    | PDF
    | SVG
    | JPG
    | EPS
    | GIF
    | TIF
    | WEBP
    deriving (Bounded, Enum, Eq, Show, Generic)

instance Hashable SaveFormat -- From Generic

instance IsString SaveFormat where
    -- An error is thrown if the save format cannot be parsed. That's OK
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

instance FromJSON SaveFormat where
    parseJSON (Object v) = fromString <$> v .: (pack . show $ SaveFormatK)
    parseJSON _ = error "Coult not parse save format"

-- | Save format file extension
extension :: SaveFormat -> String
extension fmt = mconcat [".", fmap toLower . show $ fmt]