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
-- Rendering Plotly-python code blocks
module Text.Pandoc.Filter.Plot.Renderers.PlotlyPython
  ( plotlyPython,
    plotlyPythonSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

plotlyPython :: PlotM Renderer
plotlyPython = do
      cmdargs <- asksConfig plotlyPythonCmdArgs
      return $
            Renderer
              { rendererToolkit = PlotlyPython,
                rendererCapture = plotlyPythonCapture,
                rendererCommand = plotlyPythonCommand cmdargs,
                rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -c "import plotly.graph_objects"|],
                rendererSupportedSaveFormats = plotlyPythonSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "python",
                rendererComment = mappend "# ",
                rendererScriptExtension = ".py"
              }

plotlyPythonSupportedSaveFormats :: [SaveFormat]
plotlyPythonSupportedSaveFormats = [PNG, JPG, WEBP, PDF, SVG, EPS, HTML]

plotlyPythonCommand :: Text -> OutputSpec -> Text
plotlyPythonCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}"|]

plotlyPythonCapture :: FigureSpec -> FilePath -> Script
plotlyPythonCapture = appendCapture plotlyPythonCaptureFragment

plotlyPythonCaptureFragment :: FigureSpec -> FilePath -> Script
plotlyPythonCaptureFragment FigureSpec {..} fname =
  [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.#{write_method}(r"#{fname}"#{extra_args})
|]
  where
    -- Note: the default behaviour for HTML export is
    --       to embed the entire Plotly.js content. This means
    --       that the resulting file can be used completely offline
    write_method = case saveFormat of
      HTML -> "write_html" :: Text
      _ -> "write_image"
    extra_args = case saveFormat of
      HTML -> ", include_plotlyjs='cdn'" :: Text
      _ -> mempty
