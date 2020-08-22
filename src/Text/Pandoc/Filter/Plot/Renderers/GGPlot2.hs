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

import qualified Data.Text as T
import           Text.Pandoc.Filter.Plot.Renderers.Prelude


ggplot2SupportedSaveFormats :: [SaveFormat]
ggplot2SupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, TIF]


ggplot2Command :: OutputSpec -> Text -> Text
ggplot2Command OutputSpec{..} exe = [st|#{exe} "#{oScriptPath}"|]


ggplot2Available :: PlotM Bool
ggplot2Available = do
    mexe <- executable GGPlot2
    case mexe of 
        Nothing -> return False
        Just (Executable dir exe) -> 
            commandSuccess dir [st|#{exe} -e 'if(!require("ggplot2")) {quit(status=1)}'|]


ggplot2Capture :: FigureSpec -> FilePath -> Script
ggplot2Capture fs fp = 
    T.unlines [ "pdf(NULL)" -- Prevent the creation of empty Rplots.pdf
              , script fs
              , ggplot2CaptureFragment fs fp
              ]


ggplot2CaptureFragment :: FigureSpec -> FilePath -> Script
ggplot2CaptureFragment FigureSpec{..} fname = [st|
library(ggplot2) # just in case
ggsave("#{toRPath fname}", plot = last_plot(), dpi = #{dpi})
|]
