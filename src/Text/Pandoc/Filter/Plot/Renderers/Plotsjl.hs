{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
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
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = Plotsjl,
                rendererExe = exe,
                rendererCapture = plotsjlCapture,
                rendererCommand = plotsjlCommand cmdargs exename,
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

plotsjlCommand :: Text -> Text -> OutputSpec -> Text
plotsjlCommand cmdargs exe OutputSpec {..} = [st|#{exe} #{cmdargs} "#{oScriptPath}"|]

plotsjlAvailable :: PlotM Bool
plotsjlAvailable = do
  mexe <- executable Plotsjl
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      withPrependedPath dir $
        asks envCWD >>= flip commandSuccess [st|#{exe} -e "using Plots"|]

plotsjlCapture :: FigureSpec -> FilePath -> Script
plotsjlCapture = appendCapture plotsjlCaptureFragment

plotsjlCaptureFragment :: FigureSpec -> FilePath -> Script
plotsjlCaptureFragment _ fname =
  [st|
savefig(raw"#{fname}")
|]
