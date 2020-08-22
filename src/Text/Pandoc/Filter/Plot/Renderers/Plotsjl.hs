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

Rendering Julia/Plots.jl code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Plotsjl (
      plotsjlSupportedSaveFormats
    , plotsjlCommand
    , plotsjlCapture
    , plotsjlAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


-- Save formats support by most backends
-- https://docs.plotsjl.org/latest/output/#Supported-output-file-formats-1
plotsjlSupportedSaveFormats :: [SaveFormat]
plotsjlSupportedSaveFormats = [PNG, SVG, PDF]


plotsjlCommand :: OutputSpec -> Text -> Text
plotsjlCommand OutputSpec{..} exe = [st|#{exe} "#{oScriptPath}"|]


plotsjlAvailable :: PlotM Bool
plotsjlAvailable = do
    mexe <- executable Plotsjl
    case mexe of 
        Nothing -> return False
        Just (Executable dir exe) -> 
            commandSuccess dir [st|#{exe} -e "using Plots"|]


plotsjlCapture :: FigureSpec -> FilePath -> Script
plotsjlCapture = appendCapture plotsjlCaptureFragment


plotsjlCaptureFragment :: FigureSpec -> FilePath -> Script
plotsjlCaptureFragment _ fname = [st|
savefig(raw"#{fname}")
|]
