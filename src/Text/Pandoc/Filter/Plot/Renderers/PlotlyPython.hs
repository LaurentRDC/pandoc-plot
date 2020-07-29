{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Rendering Plotly-python code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.PlotlyPython (
      plotlyPythonSupportedSaveFormats
    , plotlyPythonCommand
    , plotlyPythonCapture
    , plotlyPythonAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


plotlyPythonSupportedSaveFormats :: [SaveFormat]
plotlyPythonSupportedSaveFormats = [PNG, JPG, WEBP, PDF, SVG, EPS, HTML]


plotlyPythonCommand :: OutputSpec -> PlotM Text
plotlyPythonCommand OutputSpec{..} = do
    exe <- executable PlotlyPython
    return [st|#{exe} "#{oScriptPath}"|]


plotlyPythonAvailable :: PlotM Bool
plotlyPythonAvailable = do
    exe <- executable PlotlyPython
    commandSuccess [st|#{exe} -c "import plotly.graph_objects"|]

plotlyPythonCapture :: FigureSpec -> FilePath -> Script
plotlyPythonCapture FigureSpec{..} fname = [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.#{write_method}(r"#{fname}"#{extra_args})
|]
    where
        -- Note: the default behaviour for HTML export is
        --       to embed the entire Plotly.js content. This means
        --       that the resulting file can be used completely offline   
        write_method = case saveFormat of
            HTML -> "write_html"::Text
            _    -> "write_image"
        extra_args = case saveFormat of
            HTML -> ", include_plotlyjs='cdn'"::Text
            _    -> mempty
