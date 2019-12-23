
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable
-}

module Text.Pandoc.Filter.Plot.Renderers (
      module Text.Pandoc.Filter.Plot.Renderers.Matplotlib
    , module Text.Pandoc.Filter.Plot.Renderers.Plotly
 ) where

import Text.Pandoc.Filter.Plot.Types                (Renderer)
import Text.Pandoc.Filter.Plot.Renderers.Matplotlib
import Text.Pandoc.Filter.Plot.Renderers.Plotly

renderers :: [Renderer]
renderers = [matplotlib, plotly]