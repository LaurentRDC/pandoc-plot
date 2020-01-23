{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $header$
Description : Pandoc filter to create figures from code blocks using your plotting toolkit of choice
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : unstable
Portability : portable

This module defines a Pandoc filter @makePlot@ and related functions
that can be used to walk over a Pandoc document and generate figures from
code blocks using a multitude of plotting toolkits.

The syntax for code blocks is simple, Code blocks with the appropriate class
attribute will trigger the filter:

*   @matplotlib@ for matplotlib-based Python plots;
*   @plotly_python@ for Plotly-based Python plots;
*   @matlabplot@ for MATLAB plots;
*   @mathplot@ for Mathematica plots;
*   @octaveplot@ for GNU Octave plots;
*   @ggplot2@ for ggplot2-based R plots;
*   @gnuplot@ for gnuplot plots;

The code block will be reworked into a script and the output figure will be captured. Optionally, the source code
 used to generate the figure will be linked in the caption.

Here are the possible attributes what pandoc-plot understands for ALL toolkits:

    * @directory=...@ : Directory where to save the figure.
    * @source=true|false@ : Whether or not to link the source code of this figure in the caption. Ideal for web pages, for example. Default is false.
    * @format=...@: Format of the generated figure. This can be an extension or an acronym, e.g. @format=PNG@.
    * @caption="..."@: Specify a plot caption (or alternate text). Captions should be formatted in the same format as the document (e.g. Markdown captions in Markdown documents).
    * @dpi=...@: Specify a value for figure resolution, or dots-per-inch. Certain toolkits ignore this.
    * @preamble=...@: Path to a file to include before the code block. Ideal to avoid repetition over many figures.

Default values for the above attributes are stored in the @Configuration@ datatype. These can be specified in a YAML file.

Here is an example code block which will render a figure using gnuplot, in Markdown:

@
    ```{.gnuplot format=png caption="Sinusoidal function"}
    sin(x)

    set xlabel "x"
    set ylabel "y"
    ```
@
-}
module Text.Pandoc.Filter.Plot (
    -- * Operating on single Pandoc blocks
      makePlot
    -- * Operating on whole Pandoc documents
    , plotTransform
    -- * Runtime configuration
    , configuration
    , Configuration(..)
    , SaveFormat(..)
    , Script
    -- * For testing purposes ONLY
    , make
    , availableToolkits
    , unavailableToolkits
    ) where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (runReaderT)

import           Data.Maybe                       (fromMaybe)

import           System.IO                        (hPutStrLn, stderr)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk                 (walkM)

import           Text.Pandoc.Filter.Plot.Internal


-- | Highest-level function that can be walked over a Pandoc tree.
-- All code blocks that have the @.plot@ / @.plotly@ class will be considered
-- figures.
--
-- The target document format determines how the figure captions should be parsed.
-- By default (i.e. if Nothing), captions will be parsed as Markdown with LaTeX math @$...$@,
makePlot :: Configuration -- ^ Configuration for default values
         -> Maybe Format  -- ^ Input document format
         -> Block 
         -> IO Block
makePlot conf mfmt block = 
    let fmt = fromMaybe (Format "markdown+tex_math_dollars") mfmt
    in maybe (return block) (\tk -> make tk conf fmt block) (plotToolkit block)


-- | Walk over an entire Pandoc document, changing appropriate code blocks
-- into figures. Default configuration is used.
plotTransform :: Configuration -- ^ Configuration for default values
              -> Maybe Format  -- ^ Input document format
              -> Pandoc 
              -> IO Pandoc
plotTransform conf mfmt = walkM $ makePlot conf mfmt


-- | Force to use a particular toolkit to render appropriate code blocks.
--
-- Failing to render a figure does not stop the filter, so that you may run the filter
-- on documents without having all necessary toolkits installed. In this case, error
-- messages are printed to stderr, and blocks are left unchanged.
make :: Toolkit 
     -> Configuration -- ^ Configuration for default values
     -> Format        -- ^ Input document format
     -> Block 
     -> IO Block
make tk conf fmt block = runReaderT (makePlot' block) (PlotEnv tk conf)
    where
        makePlot' :: Block -> PlotM Block
        makePlot' blk 
            = parseFigureSpec blk 
            >>= maybe 
                    (return blk) 
                    (\s -> runScriptIfNecessary s >>= handleResult s)
            where
                handleResult spec ScriptSuccess         = return $ toImage fmt spec
                handleResult _ (ScriptChecksFailed msg) = do
                    liftIO $ hPutStrLn stderr $ "pandoc-plot: The script check failed with message: " <> msg 
                    return blk
                handleResult _ (ScriptFailure _ code) = do
                    liftIO $ hPutStrLn stderr $ "pandoc-plot: The script failed with exit code " <> show code 
                    return blk
                

-- | Possible errors returned by the filter
data PandocPlotError
    = ScriptError String Int          -- ^ Running script has yielded an error
    | ScriptChecksFailedError String  -- ^ Script did not pass all checks
    deriving (Eq)

instance Show PandocPlotError where
    show (ScriptError cmd exitcode)   = mconcat [ "Script error: plot could not be generated.\n"
                                                , "    Command: ", cmd, "\n"
                                                , "    Exit code " <> (show exitcode)
                                                ]
    show (ScriptChecksFailedError msg) = "Script did not pass all checks: " <> msg