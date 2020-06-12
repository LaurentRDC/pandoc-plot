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

Rendering Plotly/R plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.PlotlyR (
      plotlyRSupportedSaveFormats
    , plotlyRCommand
    , plotlyRCapture
    , plotlyRAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


plotlyRSupportedSaveFormats :: [SaveFormat]
plotlyRSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS]


plotlyRCommand :: OutputSpec -> PlotM Text
plotlyRCommand OutputSpec{..} = do
    exe <- executable PlotlyR
    return [st|#{exe} "#{oScriptPath}"|]


plotlyRAvailable :: PlotM Bool
plotlyRAvailable = do
    exe <- executable GGPlot2
    commandSuccess [st|#{exe} -e 'library("plotly")'|]


-- Based on the following documentation:
--    https://plotly.com/r/static-image-export/
plotlyRCapture :: FigureSpec -> FilePath -> Script
plotlyRCapture FigureSpec{..} fname = [st|
library(plotly) # just in case
if (!require("processx")) install.packages("processx")
orca(last_plot(), file = "#{fname}")
|]
