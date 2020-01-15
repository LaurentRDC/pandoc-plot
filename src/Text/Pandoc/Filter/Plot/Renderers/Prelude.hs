
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
    , Text
    , st
    , unpack
) where

import Data.Text                       (Text, unpack)
import Text.Shakespeare.Text           (st)

import Text.Pandoc.Filter.Plot.Types