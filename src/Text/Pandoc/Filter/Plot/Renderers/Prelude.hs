{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Prelude for renderers, containing some helpful utilities.
-}

module Text.Pandoc.Filter.Plot.Renderers.Prelude (

      module Prelude
    , module Text.Pandoc.Filter.Plot.Types
    , module Data.Yaml
    , MonadReader
    , ReaderT
    , MonadIO
    , Text
    , Default
    , asks
    , def
    , runReaderT
    , unpack
    , st
) where

import Control.Monad.Reader            (ReaderT, MonadIO, runReaderT, asks)
import Control.Monad.Reader.Class      (MonadReader)

import Data.Default.Class              (Default, def)
import Data.Text                       (Text, unpack)
import Data.Yaml                        
import Text.Shakespeare.Text           (st)

import Text.Pandoc.Filter.Plot.Types