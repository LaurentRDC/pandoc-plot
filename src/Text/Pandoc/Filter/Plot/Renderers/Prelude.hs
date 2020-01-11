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
    , HasBaseConfig(..)
    , HasPreamble(..)
    , loadConfig
    , def
    , runReaderT
    , unpack
    , st
) where

import Control.Monad.Reader            (ReaderT, MonadIO, runReaderT)
import Control.Monad.Reader.Class      (MonadReader)

import Data.Default.Class              (Default, def)
import Data.Text                       (Text, unpack)
import Data.Yaml                        
import Data.Yaml.Config                (ignoreEnv, loadYamlSettings)
import Text.Shakespeare.Text           (st)

import Text.Pandoc.Filter.Plot.Types


-- | Lens-like access to base configuration. 
-- This makes writing instances of PlotConfig trivial.
class HasBaseConfig c where
    baseConfig :: c -> BaseConfig


-- | Lens-like access to base configuration. 
-- This makes writing instances of PlotConfig trivial.
class HasPreamble c where
    ppreamble :: c -> Script


instance (FromJSON c, Default c, HasBaseConfig c, HasPreamble c) => PlotConfig c where
    defaultDirectory    = bdefaultDirectory  . baseConfig
    defaultWithLinks    = bdefaultWithLinks  . baseConfig
    defaultDPI          = bdefaultDPI        . baseConfig
    defaultSaveFormat   = bdefaultSaveFormat . baseConfig
    pythonInterpreter   = bpythonInterpreter . baseConfig
    preamble            = ppreamble


loadConfig :: PlotConfig c => FilePath -> IO c
loadConfig fp = loadYamlSettings [fp] [] ignoreEnv