{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : $header$
-- Description : Pandoc filter to create figures from code blocks using your plotting toolkit of choice
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
--
-- This module defines a Pandoc filter @plotTransform@ and related functions
-- that can be used to walk over a Pandoc document and generate figures from
-- code blocks, using a multitude of plotting toolkits.
--
-- The syntax for code blocks is simple. Code blocks with the appropriate class
-- attribute will trigger the filter, e.g. @matplotlib@ for matplotlib-based Python plots.
--
-- Here is an example, in Markdown, for a plot in MATLAB:
--
-- @
--     This is a paragraph.
--
--     ```{.matlabplot}
--     figure()
--     plot([1,2,3,4,5], [1,2,3,4,5], '-k')
--     ```
-- @
--
-- The code block will be reworked into a script and the output figure will be captured. Optionally, the source code
--  used to generate the figure will be linked in the caption.
--
-- Here are the possible attributes what pandoc-plot understands for ALL toolkits:
--
--     * @directory=...@ : Directory where to save the figure. This path should be specified with
--       respect to the current working directory, and not with respect to the document.
--     * @source=true|false@ : Whether or not to link the source code of this figure in the caption.
--       Ideal for web pages, for example. Default is false.
--     * @format=...@: Format of the generated figure. This can be an extension or an acronym,
--       e.g. @format=PNG@.
--     * @caption="..."@: Specify a plot caption (or alternate text). Format
--       for captions is specified in the documentation for the @Configuration@ type.
--     * @dpi=...@: Specify a value for figure resolution, or dots-per-inch. Certain toolkits ignore this.
--     * @dependencies=[...]@: Specify files/directories on which a figure depends, e.g. data file.
--       Figures will be re-rendered if one of those file/directory changes. These paths should
--       be specified with respect to the current working directory, and not with respect to the document.
--     * @preamble=...@: Path to a file to include before the code block. Ideal to avoid repetition over
--       many figures.
--     * @file=...@: Path to a file from which to read the content of the figure. The content of the
--       code block will be ignored. This path should be specified with respect to the current working
--       directory, and not with respect to the document.
--
-- Default values for the above attributes are stored in the @Configuration@ datatype. These can be specified in a
-- YAML file.
--
-- Here is an example code block which will render a figure using gnuplot, in Markdown:
--
-- @
--     ```{.gnuplot format=png caption="Sinusoidal function" source=true}
--     sin(x)
--
--     set xlabel "x"
--     set ylabel "y"
--     ```
-- @
module Text.Pandoc.Filter.Plot
  ( -- * Operating on whole Pandoc documents
    plotTransform,

    -- * Cleaning output directories
    cleanOutputDirs,

    -- * Runtime configuration
    configuration,
    defaultConfiguration,
    Configuration (..),
    Verbosity (..),
    LogSink (..),
    SaveFormat (..),
    Script,

    -- * Determining available plotting toolkits
    Toolkit (..),
    availableToolkits,
    unavailableToolkits,
    toolkits,
    supportedSaveFormats,

    -- * Version information
    pandocPlotVersion,

    -- * For embedding, testing and internal purposes ONLY. Might change without notice.
    make,
    makeEither,
    PandocPlotError (..),
  )
where

import Control.Concurrent (getNumCapabilities)
import Data.Functor ((<&>))
import Data.Map (singleton)
import Data.Text (Text, pack, unpack)
import Data.Version (Version)
import Paths_pandoc_plot (version)
import Text.Pandoc.Definition (Block, Meta (..), MetaValue (..), Pandoc (..))
import Text.Pandoc.Filter.Plot.Internal
  ( Configuration (..),
    FigureSpec,
    LogSink (..),
    ParseFigureResult (..),
    PlotM,
    RuntimeEnv (envConfig),
    SaveFormat (..),
    Script,
    ScriptResult (..),
    Toolkit (..),
    Verbosity (..),
    asks,
    availableToolkits,
    cleanOutputDirs,
    configuration,
    debug,
    defaultConfiguration,
    mapConcurrentlyN,
    parseFigureSpec,
    runPlotM,
    runScriptIfNecessary,
    supportedSaveFormats,
    throwStrictError,
    toFigure,
    toolkits,
    unavailableToolkits,
    whenStrict,
  )
import Text.Pandoc.Walk (walkM)

-- | Walk over an entire Pandoc document, transforming appropriate code blocks
-- into figures. This function will operate on blocks in parallel if possible.
--
-- Failing to render a figure does not stop the filter, so that you may run the filter
-- on documents without having all necessary toolkits installed. In this case, error
-- messages are printed to stderr, and blocks are left unchanged.
plotTransform ::
  -- | Configuration for default values
  Configuration ->
  -- | Input document
  Pandoc ->
  IO Pandoc
plotTransform conf (Pandoc meta blocks) = do
  maxproc <- getNumCapabilities
  -- TODO: make filter aware of target format
  runPlotM Nothing conf $ do
    debug $ mconcat ["Starting a new run, utilizing at most ", pack . show $ maxproc, " processes."]
    mapConcurrentlyN maxproc make blocks <&> Pandoc newMeta
  where
    newMeta = meta <> Meta (singleton "graphics" $ MetaBool True)

-- | The version of the pandoc-plot package.
--
-- @since 0.8.0.0
pandocPlotVersion :: Version
pandocPlotVersion = version

-- | Try to process the block with `pandoc-plot`. If a failure happens (or the block)
-- was not meant to become a figure, return the block as-is unless running in strict mode.
--
-- New in version 1.2.0: this function will detect nested code blocks, for example in @Div@ blocks.
make :: Block -> PlotM Block
make = walkM $ \blk -> either (onError blk) return =<< makeEither blk
  where
    onError :: Block -> PandocPlotError -> PlotM Block
    onError b e = do
      whenStrict $ throwStrictError (pack . show $ e)
      return b

-- | Try to process the block with `pandoc-plot`, documenting the error.
-- This function does not transform code blocks nested in
-- other blocks (e.g. @Divs@)
makeEither :: Block -> PlotM (Either PandocPlotError Block)
makeEither block =
  parseFigureSpec block
    >>= \case
      NotAFigure -> return $ Right block
      Figure fs -> runScriptIfNecessary fs >>= handleResult fs
      MissingToolkit tk -> return $ Left $ ToolkitNotInstalledError tk
  where
    -- Logging of errors has been taken care of in @runScriptIfNecessary@
    handleResult :: FigureSpec -> ScriptResult -> PlotM (Either PandocPlotError Block)
    handleResult _ (ScriptFailure cmd code _) = return $ Left (ScriptRuntimeError cmd code)
    handleResult _ (ScriptChecksFailed msg) = return $ Left (ScriptChecksFailedError msg)
    handleResult spec ScriptSuccess = asks envConfig >>= \c -> Right <$> toFigure (captionFormat c) spec

data PandocPlotError
  = ScriptRuntimeError Text Int
  | ScriptChecksFailedError Text
  | ToolkitNotInstalledError Toolkit

instance Show PandocPlotError where
  show (ScriptRuntimeError _ exitcode) = "ERROR (pandoc-plot) The script failed with exit code " <> show exitcode <> "."
  show (ScriptChecksFailedError msg) = "ERROR (pandoc-plot) A script check failed with message: " <> unpack msg <> "."
  show (ToolkitNotInstalledError tk) = "ERROR (pandoc-plot) The " <> show tk <> " toolkit is required but not installed."
