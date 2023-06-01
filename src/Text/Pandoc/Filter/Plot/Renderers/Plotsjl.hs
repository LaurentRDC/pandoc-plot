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
-- Rendering Julia/Plots.jl code blocks
module Text.Pandoc.Filter.Plot.Renderers.Plotsjl
  ( plotsjl,
    plotsjlSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

plotsjl :: PlotM Renderer
plotsjl = do
  cmdargs <- asksConfig plotsjlCmdArgs
  return
    $ Renderer
      { rendererToolkit = Plotsjl,
        rendererCapture = plotsjlCapture,
        rendererCommand = plotsjlCommand cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -e "using Plots"|],
        rendererSupportedSaveFormats = plotsjlSupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "julia",
        rendererComment = mappend "# ",
        rendererScriptExtension = ".jl"
      }

-- Save formats support by most backends
-- https://docs.plotsjl.org/latest/output/#Supported-output-file-formats-1
plotsjlSupportedSaveFormats :: [SaveFormat]
plotsjlSupportedSaveFormats = [PNG, SVG, PDF]

plotsjlCommand :: Text -> OutputSpec -> Text
plotsjlCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} -- "#{oScriptPath}"|]

plotsjlCapture :: FigureSpec -> FilePath -> Script
plotsjlCapture = appendCapture plotsjlCaptureFragment

plotsjlCaptureFragment :: FigureSpec -> FilePath -> Script
plotsjlCaptureFragment _ fname =
  [st|
savefig(raw"#{fname}")
|]
