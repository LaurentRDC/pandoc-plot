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

Rendering Octave plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Octave (
      octaveSupportedSaveFormats
    , octaveCommand
    , octaveCapture
    , octaveAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


octaveSupportedSaveFormats :: [SaveFormat]
octaveSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]


octaveCommand :: OutputSpec -> PlotM (FilePath, Text)
octaveCommand OutputSpec{..} = do
    (dir, exe) <- executable Octave
    return (dir, [st|#{exe} --no-gui --no-window-system "#{oScriptPath}"|])


octaveAvailable :: PlotM Bool
octaveAvailable = do
    (dir, exe) <- executable Octave
    commandSuccess dir [st|#{exe} -h|]


octaveCapture :: FigureSpec -> FilePath -> Script
octaveCapture = appendCapture octaveCaptureFragment


octaveCaptureFragment :: FigureSpec -> FilePath -> Script
octaveCaptureFragment _ fname = [st|
saveas(gcf, '#{fname}')
|]
