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

Rendering Matlab code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Matlab (
      matlabSupportedSaveFormats
    , matlabCommand
    , matlabCapture
    , matlabAvailable
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


matlabSupportedSaveFormats :: [SaveFormat]
matlabSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]


matlabCommand :: Configuration -> FigureSpec -> FilePath -> Text
matlabCommand Configuration{..} _ fp = [st|#{matlabExe} -batch "run('#{fp}')"|]


matlabAvailable :: Configuration -> IO Bool
matlabAvailable Configuration{..} = commandSuccess [st|#{matlabExe} -h|]

matlabCapture :: FigureSpec -> FilePath -> Script
matlabCapture FigureSpec{..} fname = [st|
saveas(gcf, '#{fname}')
|]
