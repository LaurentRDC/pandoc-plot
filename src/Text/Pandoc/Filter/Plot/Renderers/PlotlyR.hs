{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Rendering Plotly/R plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.PlotlyR
  ( plotlyR,
    plotlyRSupportedSaveFormats,
  )
where

import qualified Data.Text as T
import Text.Pandoc.Filter.Plot.Renderers.Prelude

plotlyR :: PlotM (Maybe Renderer)
plotlyR = do
  avail <- plotlyRAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig plotlyRCmdArgs
      mexe <- executable PlotlyR
      return $
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = PlotlyR,
                rendererExe = exe,
                rendererCapture = plotlyRCapture,
                rendererCommand = plotlyRCommand cmdargs exename,
                rendererSupportedSaveFormats = plotlyRSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "r",
                rendererComment = mappend "# ",
                rendererScriptExtension = ".r"
              }

plotlyRSupportedSaveFormats :: [SaveFormat]
plotlyRSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, HTML]

plotlyRCommand :: Text -> Text -> OutputSpec -> Text
plotlyRCommand cmdargs exe OutputSpec {..} = [st|#{exe} #{cmdargs} "#{oScriptPath}"|]

plotlyRAvailable :: PlotM Bool
plotlyRAvailable = do
  mexe <- executable PlotlyR
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      withPrependedPath dir $
        asks envCWD >>= flip commandSuccess [st|#{exe} -e "if(!require('plotly')) {quit(status=1)}"|]

plotlyRCapture :: FigureSpec -> FilePath -> Script
plotlyRCapture fs fp =
  T.unlines
    [ "pdf(NULL)", -- Prevent the creation of empty Rplots.pdf
      script fs,
      plotlyRCaptureFragment fs fp
    ]

plotlyRCaptureFragment :: FigureSpec -> FilePath -> Script
plotlyRCaptureFragment spec@FigureSpec {..} fname = case saveFormat of
  HTML -> plotlyRCaptureHtml spec fname
  _ -> plotlyRCaptureStatic spec fname

-- Based on the following discussion:
--    https://stackoverflow.com/q/34580095
plotlyRCaptureHtml :: FigureSpec -> FilePath -> Script
plotlyRCaptureHtml _ fname =
  [st|
library(plotly) # just in case
library(htmlwidgets)
p <- last_plot()
htmlwidgets::saveWidget(as_widget(p), "#{toRPath fname}")
|]

-- Based on the following documentation:
--    https://plotly.com/r/static-image-export/
plotlyRCaptureStatic :: FigureSpec -> FilePath -> Script
plotlyRCaptureStatic _ fname =
  [st|
library(plotly) # just in case
if (!require("processx")) install.packages("processx")
pdf(NULL)
orca(last_plot(), file = "#{toRPath fname}")
|]
