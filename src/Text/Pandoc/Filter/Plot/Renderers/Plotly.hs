{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable
-}

module Text.Pandoc.Filter.Plot.Renderers.Plotly (plotly) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

plotly :: Renderer
plotly = Renderer {
      rendererName = "plotly"
    , rendererSaveFormats = [PNG, JPG, WEBP, PDF, SVG, EPS]
    , capture = capturePlotly
}

-- We are trying to emulate the behavior of @matplotlib.pyplot.savefig@ which
-- knows the "current figure". This saves us from contraining users to always
-- have the same Plotly figure name, e.g. @fig@ in all examples on plot.ly
capturePlotly :: FigureSpec -> FilePath -> Script
capturePlotly _ fname = [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.write_image("#{fname}")
|]