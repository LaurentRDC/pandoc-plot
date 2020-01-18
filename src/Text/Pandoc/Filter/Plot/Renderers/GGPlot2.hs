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

module Text.Pandoc.Filter.Plot.Renderers.GGPlot2 (
      ggplot2SupportedSaveFormats
    , ggplot2Command
    , ggplot2Capture
) where

import           Text.Pandoc.Filter.Plot.Renderers.Prelude

ggplot2SupportedSaveFormats :: [SaveFormat]
ggplot2SupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, TIF]


ggplot2Command :: Configuration -> FigureSpec -> FilePath -> Text
ggplot2Command Configuration{..} _ fp = [st|#{ggplot2Exe} #{fp}|]


ggplot2Capture :: FigureSpec -> FilePath -> Script
ggplot2Capture FigureSpec{..} fname = [st|
library(ggplot2) # just in case
ggsave("#{fname}", plot = last_plot(), dpi = #{dpi})
|]
