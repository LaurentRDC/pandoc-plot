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

plotlyR :: PlotM Renderer
plotlyR = do
      cmdargs <- asksConfig plotlyRCmdArgs
      return $
        Renderer
          { rendererToolkit = PlotlyR,
            rendererCapture = plotlyRCapture,
            rendererCommand = plotlyRCommand cmdargs,
            rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -e "if(!require('plotly')) {quit(status=1)}"|],
            rendererSupportedSaveFormats = plotlyRSupportedSaveFormats,
            rendererChecks = mempty,
            rendererLanguage = "r",
            rendererComment = mappend "# ",
            rendererScriptExtension = ".r"
          }

plotlyRSupportedSaveFormats :: [SaveFormat]
plotlyRSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, HTML]

plotlyRCommand :: Text -> OutputSpec -> Text
plotlyRCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}"|]

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
