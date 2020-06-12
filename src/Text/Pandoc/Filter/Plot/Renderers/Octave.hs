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


octaveCommand :: OutputSpec -> PlotM Text
octaveCommand OutputSpec{..} = do
    exe <- executable Octave
    return [st|#{exe} --no-gui --no-window-system "#{oScriptPath}"|]


octaveAvailable :: PlotM Bool
octaveAvailable = do
    exe <- executable Octave
    commandSuccess [st|#{exe} -h|]


octaveCapture :: FigureSpec -> FilePath -> Script
octaveCapture FigureSpec{..} fname = [st|
saveas(gcf, '#{fname}')
|]
