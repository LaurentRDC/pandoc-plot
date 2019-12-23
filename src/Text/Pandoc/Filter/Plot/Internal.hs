
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module re-exports internal pandoc-plot functionality.
-}

module Text.Pandoc.Filter.Plot.Internal (
      module Text.Pandoc.Filter.Plot.Types
    , module Text.Pandoc.Filter.Plot.Configuration 
 ) where

import Text.Pandoc.Filter.Plot.Types
import Text.Pandoc.Filter.Plot.Configuration
