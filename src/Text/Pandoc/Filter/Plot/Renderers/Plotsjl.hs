{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2020
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

plotsjl :: PlotM (Maybe Renderer)
plotsjl = do
  avail <- plotsjlAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig plotsjlCmdArgs
      mexe <- executable Plotsjl
      return $
        mexe >>= \exe ->
          return
            Renderer
              { rendererToolkit = Plotsjl,
                rendererExe = exe,
                rendererCmdArgs = cmdargs,
                rendererCapture = plotsjlCapture,
                rendererCommand = plotsjlCommand,
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

plotsjlCommand :: Text -> OutputSpec -> Text -> Text
plotsjlCommand cmdargs OutputSpec {..} exe = [st|#{exe} #{cmdargs} "#{oScriptPath}"|]

plotsjlAvailable :: PlotM Bool
plotsjlAvailable = do
  mexe <- executable Plotsjl
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      commandSuccess dir [st|#{exe} -e "using Plots"|]

plotsjlCapture :: FigureSpec -> FilePath -> Script
plotsjlCapture = appendCapture plotsjlCaptureFragment

plotsjlCaptureFragment :: FigureSpec -> FilePath -> Script
plotsjlCaptureFragment _ fname =
  [st|
savefig(raw"#{fname}")
|]
