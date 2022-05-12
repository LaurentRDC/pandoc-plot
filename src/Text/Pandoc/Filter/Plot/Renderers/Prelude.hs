{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
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
    findExecutable,
    appendCapture,
    toRPath,
  )
where

import Data.Text (Text, unpack)
import System.Directory (findExecutable)
import System.FilePath (isPathSeparator)
import Text.Pandoc.Filter.Plot.Monad
import Text.Shakespeare.Text (st)

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
