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

Rendering Plotly code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Plotly (
      plotlyPythonSupportedSaveFormats
    , plotlyPythonCommand
    , plotlyPythonCapture
    , plotlyPythonAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


plotlyPythonSupportedSaveFormats :: [SaveFormat]
plotlyPythonSupportedSaveFormats = [PNG, JPG, WEBP, PDF, SVG, EPS]


plotlyPythonCommand :: Configuration -> FigureSpec -> FilePath -> Text
plotlyPythonCommand Configuration{..} _ fp = [st|#{plotlyPythonExe} "#{fp}"|]


plotlyPythonAvailable :: Configuration -> IO Bool
plotlyPythonAvailable Configuration{..} = commandSuccess [st|#{plotlyPythonExe} -c "import plotly.graph_objects"|]


plotlyPythonCapture :: FigureSpec -> FilePath -> Script
plotlyPythonCapture _ fname = [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.write_image(r"#{fname}")
|]
