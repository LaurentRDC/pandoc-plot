{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $header$
Description : Pandoc filter to create figures from code blocks using your plotting toolkit of choice
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : unstable
Portability : portable

This module defines a Pandoc filter @plotTransform@ and related functions
that can be used to walk over a Pandoc document and generate figures from
code blocks, using a multitude of plotting toolkits.

The syntax for code blocks is simple. Code blocks with the appropriate class
attribute will trigger the filter:

*   @matplotlib@ for matplotlib-based Python plots;
*   @plotly_python@ for Plotly-based Python plots;
*   @matlabplot@ for MATLAB plots;
*   @mathplot@ for Mathematica plots;
*   @octaveplot@ for GNU Octave plots;
*   @ggplot2@ for ggplot2-based R plots;
*   @gnuplot@ for gnuplot plots;

For example, in Markdown:

@
    This is a paragraph.

    ```{.matlabplot}
    figure()
    plot([1,2,3,4,5], [1,2,3,4,5], '-k)
    ```
@

The code block will be reworked into a script and the output figure will be captured. Optionally, the source code
 used to generate the figure will be linked in the caption.

Here are the possible attributes what pandoc-plot understands for ALL toolkits:

    * @directory=...@ : Directory where to save the figure.
    * @source=true|false@ : Whether or not to link the source code of this figure in the caption. Ideal for web pages, for example. Default is false.
    * @format=...@: Format of the generated figure. This can be an extension or an acronym, e.g. @format=PNG@.
    * @caption="..."@: Specify a plot caption (or alternate text). Format for captions is specified in the documentation for the @Configuration@ type.
    * @dpi=...@: Specify a value for figure resolution, or dots-per-inch. Certain toolkits ignore this.
    * @preamble=...@: Path to a file to include before the code block. Ideal to avoid repetition over many figures.

Default values for the above attributes are stored in the @Configuration@ datatype. These can be specified in a 
YAML file which must be named ".pandoc-plot.yml".

Here is an example code block which will render a figure using gnuplot, in Markdown:

@
    ```{.gnuplot format=png caption="Sinusoidal function" source=true}
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
    , parPlotTransform
    -- * Cleaning output directories
    , cleanOutputDirs
    -- * Runtime configuration
    , configuration
    , Configuration(..)
    , SaveFormat(..)
    , Script
    -- * For testing and internal purposes ONLY
    , make
    , readDoc
    , availableToolkits
    , unavailableToolkits
    ) where

import Control.Concurrent                   (getNumCapabilities)
import Control.Concurrent.ParallelIO.Local  (withPool, parallel)

import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Reader                 (runReaderT)

import System.IO                            (hPutStrLn, stderr)

import Text.Pandoc.Definition
import Text.Pandoc.Walk                     (walkM)

import Text.Pandoc.Filter.Plot.Internal


-- | Highest-level function that can be walked over a Pandoc tree.
-- All code blocks that have the appropriate class names will be considered
-- figures, e.g. @.matplotlib@.
--
-- Failing to render a figure does not stop the filter, so that you may run the filter
-- on documents without having all necessary toolkits installed. In this case, error
-- messages are printed to stderr, and blocks are left unchanged.
makePlot :: Configuration -- ^ Configuration for default values
         -> Block 
         -> IO Block
makePlot conf block = maybe (return block) (\tk -> make tk conf block) (plotToolkit block)


-- | Walk over an entire Pandoc document, transforming appropriate code blocks
-- into figures. 
--
-- Failing to render a figure does not stop the filter, so that you may run the filter
-- on documents without having all necessary toolkits installed. In this case, error
-- messages are printed to stderr, and blocks are left unchanged.
plotTransform :: Configuration -- ^ Configuration for default values
              -> Pandoc        -- ^ Input document
              -> IO Pandoc
plotTransform conf = walkM $ makePlot conf


-- | Walk over an entire Pandoc document *in parallel*, transforming appropriate code blocks
-- into figures.
--
-- Failing to render a figure does not stop the filter, so that you may run the filter
-- on documents without having all necessary toolkits installed. In this case, error
-- messages are printed to stderr, and blocks are left unchanged.
--
-- @since 0.5.0.0
parPlotTransform :: Configuration 
               -> Pandoc
               -> IO Pandoc
parPlotTransform conf (Pandoc meta blocks) = do
    -- We use the maximum number of threads possible, up until
    -- the point where there are more threads than blocks
    availableThreads <- getNumCapabilities
    let numThreads = min availableThreads (length blocks)

    newBlocks <- withPool numThreads $ \pool -> parallel pool (makePlot conf <$> blocks)
    return $ Pandoc meta newBlocks


-- | Force to use a particular toolkit to render appropriate code blocks.
--
-- Failing to render a figure does not stop the filter, so that you may run the filter
-- on documents without having all necessary toolkits installed. In this case, error
-- messages are printed to stderr, and blocks are left unchanged.
make :: Toolkit       -- ^ Plotting toolkit.
     -> Configuration -- ^ Configuration for default values.
     -> Block 
     -> IO Block
make tk conf block = runReaderT (makePlot' block) (PlotEnv tk conf)
    where
        makePlot' :: Block -> PlotM Block
        makePlot' blk 
            = parseFigureSpec blk 
            >>= maybe 
                    (return blk) 
                    (\s -> runScriptIfNecessary s >>= handleResult s)
            where                
                handleResult spec ScriptSuccess         = return $ toImage (captionFormat conf) spec
                handleResult _ (ScriptChecksFailed msg) = do
                    liftIO $ hPutStrLn stderr $ " ERROR (pandoc-plot) The script check failed with message: " <> msg 
                    return blk
                handleResult _ (ScriptFailure _ code) = do
                    liftIO $ hPutStrLn stderr $ "ERROR (pandoc-plot) The script failed with exit code " <> show code 
                    return blk
                handleResult _ (ToolkitNotInstalled tk') = do
                    liftIO $ hPutStrLn stderr $ "ERROR (pandoc-plot) The " <> show tk' <> " toolkit is required but not installed."
                    return blk