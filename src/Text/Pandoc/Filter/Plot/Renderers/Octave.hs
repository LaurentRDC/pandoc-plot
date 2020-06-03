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


octaveCommand :: Configuration -> FigureSpec -> FilePath -> IO Text
octaveCommand conf _ fp = do
    exe <- executable Octave conf
    return [st|#{exe} --no-gui --no-window-system "#{fp}"|]


octaveAvailable :: Configuration -> IO Bool
octaveAvailable conf = do
    exe <- executable Octave conf
    commandSuccess [st|#{exe} -h|]


octaveCapture :: FigureSpec -> FilePath -> Script
octaveCapture FigureSpec{..} fname = [st|
saveas(gcf, '#{fname}')
|]
