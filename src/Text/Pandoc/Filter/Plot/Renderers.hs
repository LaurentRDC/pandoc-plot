{-# LANGUAGE OverloadedStrings #-}

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
    , availableToolkits
    , unavailableToolkits
) where

import           Control.Monad                                 (filterM)

import           Data.List                                     ((\\))
import           Data.Map.Strict                               (Map)
import           Data.Maybe                                    (isJust)
import           Data.Text                                     (Text)

import qualified Turtle                                        as Sh

import           Text.Pandoc.Filter.Plot.Renderers.Mathematica
import           Text.Pandoc.Filter.Plot.Renderers.Matlab
import           Text.Pandoc.Filter.Plot.Renderers.Matplotlib
import           Text.Pandoc.Filter.Plot.Renderers.Octave
import           Text.Pandoc.Filter.Plot.Renderers.Plotly

import           Text.Pandoc.Filter.Plot.Types

-- Extension for script files, e.g. ".py", or ".m".
scriptExtension :: Toolkit -> String
scriptExtension Matplotlib   = ".py"
scriptExtension PlotlyPython = ".py"
scriptExtension Matlab       = ".m"
scriptExtension Mathematica  = ".m"
scriptExtension Octave       = ".m"

-- Make a string into a comment
comment :: Toolkit -> (Text -> Text)
comment Matplotlib   = mappend "# "
comment PlotlyPython = mappend "# "
comment Matlab       = mappend "% "
comment Mathematica  = \t -> mconcat ["(*", t, "*)"]
comment Octave       = mappend "% "

-- The function that maps from configuration to the preamble.
preambleSelector :: Toolkit -> (Configuration -> Script)
preambleSelector Matplotlib   = matplotlibPreamble
preambleSelector PlotlyPython = plotlyPythonPreamble
preambleSelector Matlab       = matlabPreamble
preambleSelector Mathematica  = mathematicaPreamble
preambleSelector Octave       = octavePreamble

-- | Save formats supported by this renderer.
supportedSaveFormats :: Toolkit -> [SaveFormat]
supportedSaveFormats Matplotlib   = matplotlibSupportedSaveFormats
supportedSaveFormats PlotlyPython = plotlyPythonSupportedSaveFormats
supportedSaveFormats Matlab       = matlabSupportedSaveFormats
supportedSaveFormats Mathematica  = mathematicaSupportedSaveFormats
supportedSaveFormats Octave       = octaveSupportedSaveFormats

-- Checks to perform before running a script. If ANY check fails,
-- the figure is not rendered. This is to prevent, for example,
-- blocking operations to occur.
scriptChecks :: Toolkit -> [Script -> CheckResult]
scriptChecks = const mempty

-- | Parse code block headers for extra attributes that are specific
-- to this renderer. By default, no extra attributes are parsed.
parseExtraAttrs :: Toolkit -> Map Text Text -> Map Text Text
parseExtraAttrs Matplotlib = matplotlibExtraAttrs
parseExtraAttrs _          = return mempty

-- | Generate the appropriate command-line command to generate a figure.
command :: Toolkit -> (FigureSpec -> FilePath -> Text)
command Matplotlib   = matplotlibCommand
command PlotlyPython = plotlyPythonCommand
command Matlab       = matlabCommand
command Mathematica  = mathematicaCommand
command Octave       = octaveCommand

-- | Script fragment required to capture a figure.
capture :: Toolkit -> (FigureSpec -> FilePath -> Script)
capture Matplotlib   = matplotlibCapture
capture PlotlyPython = plotlyPythonCapture
capture Matlab       = matlabCapture
capture Mathematica  = mathematicaCapture
capture Octave       = octaveCapture


-- | List of toolkits available on this machine.
availableToolkits :: IO [Toolkit]
availableToolkits = filterM toolkitAvailable toolkits
    where
        toolkitAvailable :: Toolkit -> IO Bool
        toolkitAvailable tk =
            Sh.which (toolkitExecutable tk) >>= (fmap isJust . return)

        -- The @which@ function from Turtle only works on
        -- windows if the executable extension is included.
        whichExt :: Text
        whichExt = if isWindows then ".exe" else mempty

        toolkitExecutable :: Toolkit -> Sh.FilePath
        toolkitExecutable Matplotlib   = Sh.fromText $ "python" <> whichExt
        toolkitExecutable PlotlyPython = Sh.fromText $ "python" <> whichExt
        toolkitExecutable Matlab       = Sh.fromText $ "matlab" <> whichExt
        toolkitExecutable Mathematica  = Sh.fromText $ "math"   <> whichExt
        toolkitExecutable Octave       = Sh.fromText $ "octave" <> whichExt

unavailableToolkits :: IO [Toolkit]
unavailableToolkits = ((\\) toolkits) <$> availableToolkits
