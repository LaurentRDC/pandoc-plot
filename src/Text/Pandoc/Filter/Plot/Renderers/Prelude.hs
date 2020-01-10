{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Rendering Matplotlib code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Prelude (

      module Prelude
    , module Text.Pandoc.Filter.Plot.Types
    , MonadReader
    , ReaderT
    , MonadIO
    , Text
    , runReaderT
    , unpack
    , st
) where

import Control.Monad.Reader            (ReaderT, MonadIO, runReaderT)
import Control.Monad.Reader.Class      (MonadReader)

import Data.Text                       (Text, unpack)
import Text.Shakespeare.Text           (st)

import Text.Pandoc.Filter.Plot.Types