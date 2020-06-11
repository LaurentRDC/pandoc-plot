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

Rendering gnuplot plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.GNUPlot (
      gnuplotSupportedSaveFormats
    , gnuplotCommand
    , gnuplotCapture
    , gnuplotAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude

gnuplotSupportedSaveFormats :: [SaveFormat]
gnuplotSupportedSaveFormats = [PNG, SVG, EPS, GIF, JPG, PDF]


gnuplotCommand :: OutputSpec -> PlotM Text
gnuplotCommand OutputSpec{..} = do
    exe <- executable GNUPlot
    return [st|#{exe} -c "#{oScriptPath}"|]


gnuplotAvailable :: PlotM Bool
gnuplotAvailable = do
    exe <- executable GNUPlot 
    liftIO $ commandSuccess [st|#{exe} -h|]


gnuplotCapture :: FigureSpec -> FilePath -> Script
gnuplotCapture FigureSpec{..} fname = [st|
set terminal #{terminalString saveFormat}
set output '#{fname}'
|]

-- | Terminal name for supported save formats
terminalString :: SaveFormat -> Text
terminalString PNG = "pngcairo"
terminalString SVG = "svg"
terminalString EPS = "postscript eps"
terminalString GIF = "gif"
terminalString JPG = "jpeg"
terminalString PDF = "pdfcairo"
terminalString fmt = error $ "gnuplot: unsupported save format" <> show fmt
