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

Rendering GGPlot2 plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.GGPlot2 (
      ggplot2SupportedSaveFormats
    , ggplot2Command
    , ggplot2Capture
    , ggplot2Available
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


ggplot2SupportedSaveFormats :: [SaveFormat]
ggplot2SupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, TIF]


ggplot2Command :: OutputSpec -> PlotM Text
ggplot2Command OutputSpec{..} = do
    exe <- executable GGPlot2
    return [st|#{exe} "#{oScriptPath}"|]


ggplot2Available :: PlotM Bool
ggplot2Available = do
    exe <- executable GGPlot2
    liftIO $ commandSuccess [st|#{exe} -e 'library("ggplot2")'|]


ggplot2Capture :: FigureSpec -> FilePath -> Script
ggplot2Capture FigureSpec{..} fname = [st|
library(ggplot2) # just in case
ggsave("#{fname}", plot = last_plot(), dpi = #{dpi})
|]
