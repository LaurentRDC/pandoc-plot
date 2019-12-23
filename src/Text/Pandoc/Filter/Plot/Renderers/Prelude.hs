{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable
-}

module Text.Pandoc.Filter.Plot.Renderers.Prelude (
      module Prelude
    , module Data.Yaml
    , module Text.Pandoc.Filter.Plot.Types
    , Default
    , IsString
    , def
    , st
)where

import Prelude

import Data.Default.Class              (Default, def)
import Data.String                     (IsString)
import Data.Yaml
import Text.Pandoc.Filter.Plot.Types
import Text.Shakespeare.Text           (st)