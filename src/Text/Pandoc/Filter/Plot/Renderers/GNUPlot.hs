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
gnuplotSupportedSaveFormats = [PNG] -- TODO: support more save formats


gnuplotCommand :: Configuration -> FigureSpec -> FilePath -> Text
gnuplotCommand Configuration{..} _ fp = [st|#{gnuplotExe} -c #{fp}|]


gnuplotAvailable :: Configuration -> IO Bool
gnuplotAvailable Configuration{..} = commandSuccess [st|#{gnuplotExe} -h|] -- TODO: test this


gnuplotCapture :: FigureSpec -> FilePath -> Script
gnuplotCapture FigureSpec{..} fname = [st|
set term png
set output '#{fname}'
|]
