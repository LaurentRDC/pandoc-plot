{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : $header$
Description : Pandoc filter to create Matplotlib/Plotly figures from code blocks
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable

This module defines a Pandoc filter @makePlot@ and related functions
that can be used to walk over a Pandoc document and generate figures from
Python code blocks.

The syntax for code blocks is simple, Code blocks with the @.plot@ or @.plotly@
attribute will trigger the filter. The code block will be reworked into a Python
script and the output figure will be captured, along with a high-resolution version
of the figure and the source code used to generate the figure.

To trigger pandoc-plot, one of the following is __required__:

    * @.matplotlib@: Trigger pandoc-plot, rendering via the Matplotlib library
    * @.plotly@: Trigger pandoc-plot, rendering via the Plotly library

Here are the possible attributes what pandoc-plot understands:

    * @directory=...@ : Directory where to save the figure.
    * @format=...@: Format of the generated figure. This can be an extension or an acronym, e.g. @format=png@.
    * @caption="..."@: Specify a plot caption (or alternate text). Captions support Markdown formatting and LaTeX math (@$...$@).
    * @dpi=...@: Specify a value for figure resolution, or dots-per-inch. Default is 80DPI. (Matplotlib only, ignored otherwise)
    * @include=...@: Path to a Python script to include before the code block. Ideal to avoid repetition over many figures.
    * @links=true|false@: Add links to source code and high-resolution version of this figure.
      This is @true@ by default, but you may wish to disable this for PDF output.

Custom configurations are possible via the @Configuration@ type and the filter
functions @plotTransformWithConfig@ and @makePlotWithConfig@.
-}
module Text.Pandoc.Filter.Plot (
    -- * Operating on single Pandoc blocks
      makePlot
    -- * Operating on whole Pandoc documents
    , plotTransform
    ) where

import           Control.Monad.Reader

import           Data.Default.Class                 (def)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk                   (walkM)

import           Text.Pandoc.Filter.Plot.Internal
import           Text.Pandoc.Filter.Plot.Scripting
import           Text.Pandoc.Filter.Plot.Parse

-- | Possible errors returned by the filter
data PandocPlotError
    = ScriptError Int                 -- ^ Running script has yielded an error
    | ScriptChecksFailedError String  -- ^ Script did not pass all checks
    deriving (Eq)

instance Show PandocPlotError where
    show (ScriptError exitcode)        = "Script error: plot could not be generated. Exit code " <> (show exitcode)
    show (ScriptChecksFailedError msg) = "Script did not pass all checks: " <> msg


-- | Highest-level function that can be walked over a Pandoc tree.
-- All code blocks that have the @.plot@ / @.plotly@ class will be considered
-- figures.
makePlot :: Configuration -> Block -> IO Block
makePlot config block =
    compose [ ((makeMatplotlib config) =<<)
            , ((makePlotly config) =<<) ] 
            (return block)


-- | Walk over an entire Pandoc document, changing appropriate code blocks
-- into figures. Default configuration is used.
plotTransform :: Configuration -> Pandoc -> IO Pandoc
plotTransform = walkM . makePlot


-- | Main routine to include plots.
-- Code blocks containing the attributes @.plot@ or @.plotly@ are considered
-- Python plotting scripts. All other possible blocks are ignored.
makePlot' :: RendererM m => Block -> m (Either PandocPlotError Block)
makePlot' block = do
    parsed <- parseFigureSpec block
    maybe 
        (return $ Right block)
        (\s -> handleResult s <$> runScriptIfNecessary s)
        parsed
    where
        handleResult _ (ScriptChecksFailed msg) = Left  $ ScriptChecksFailedError msg
        handleResult _ (ScriptFailure code)     = Left  $ ScriptError code
        handleResult spec ScriptSuccess         = Right $ toImage spec


makeMatplotlib :: Configuration -> Block -> IO Block
makeMatplotlib config block = 
    runMatplotlib config (makePlot' block)
    >>= either (fail . show) return


makePlotly :: Configuration -> Block -> IO Block
makePlotly config block = 
    runPlotly config (makePlot' block)
    >>= either (fail . show) return


-- Compose a list of functions
compose :: [r -> r] -> r -> r
compose = flip (foldl (flip id))
