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

Rendering Mathematica plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Mathematica (
      mathematicaSupportedSaveFormats
    , mathematicaCommand
    , mathematicaCapture
    , mathematicaAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude

mathematicaSupportedSaveFormats :: [SaveFormat]
mathematicaSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]


mathematicaCommand :: OutputSpec -> PlotM (FilePath, Text)
mathematicaCommand OutputSpec{..} = do
    (dir, exe) <- executable Mathematica
    return (dir, [st|#{exe} -script "#{oScriptPath}"|])


mathematicaAvailable :: PlotM Bool
mathematicaAvailable = do
    (dir, exe) <- executable Mathematica
    commandSuccess dir [st|#{exe} -h|] -- TODO: test this


mathematicaCapture :: FigureSpec -> FilePath -> Script
mathematicaCapture = appendCapture mathematicaCaptureFragment


mathematicaCaptureFragment :: FigureSpec -> FilePath -> Script
mathematicaCaptureFragment FigureSpec{..} fname = [st|
Export["#{fname}", %, #{show saveFormat}, ImageResolution -> #{dpi}]
|]
