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
plotlyPythonSupportedSaveFormats = [PNG, JPG, WEBP, PDF, SVG, EPS]


plotlyPythonCommand :: OutputSpec -> PlotM Text
plotlyPythonCommand OutputSpec{..} = do
    exe <- executable PlotlyPython
    return [st|#{exe} "#{oScriptPath}"|]


plotlyPythonAvailable :: PlotM Bool
plotlyPythonAvailable = do
    exe <- executable PlotlyPython
    commandSuccess [st|#{exe} -c "import plotly.graph_objects"|]


plotlyPythonCapture :: FigureSpec -> FilePath -> Script
plotlyPythonCapture _ fname = [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.write_image(r"#{fname}")
|]
