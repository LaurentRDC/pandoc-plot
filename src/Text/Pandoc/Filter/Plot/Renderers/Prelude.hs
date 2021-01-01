{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Prelude for renderers, containing some helpful utilities.
module Text.Pandoc.Filter.Plot.Renderers.Prelude
  ( module Prelude,
    module Text.Pandoc.Filter.Plot.Monad,
    Text,
    st,
    unpack,
    commandSuccess,
    existsOnPath,
    appendCapture,
    toRPath,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (isPathSeparator)
import Text.Pandoc.Filter.Plot.Monad
import Text.Shakespeare.Text (st)

-- | Check that the supplied command results in
-- an exit code of 0 (i.e. no errors)
commandSuccess ::
  FilePath -> -- Directory from which to run the command
  Text -> -- Command to run, including the executable
  PlotM Bool
commandSuccess fp s = do
  (ec, _) <- runCommand fp s
  return $ ec == ExitSuccess

-- | Checks that an executable is available on path, at all.
existsOnPath :: FilePath -> IO Bool
existsOnPath fp = findExecutable fp >>= fmap isJust . return

-- | A shortcut to append capture script fragments to scripts
appendCapture ::
  (FigureSpec -> FilePath -> Script) ->
  FigureSpec ->
  FilePath ->
  Script
appendCapture f s fp = mconcat [script s, "\n", f s fp]

-- | R paths use the '/' path separator
toRPath :: FilePath -> FilePath
toRPath = fmap (\c -> if isPathSeparator c then '/' else c)
