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
-- Rendering gnuplot plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.GNUPlot
  ( gnuplot,
    gnuplotSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

gnuplot :: PlotM (Maybe Renderer)
gnuplot = do
  avail <- gnuplotAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig gnuplotCmdArgs
      mexe <- executable GNUPlot
      return $
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = GNUPlot,
                rendererExe = exe,
                rendererCapture = gnuplotCapture,
                rendererCommand = gnuplotCommand cmdargs exename,
                rendererSupportedSaveFormats = gnuplotSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "gnuplot",
                rendererComment = mappend "# ",
                rendererScriptExtension = ".gp"
              }

gnuplotSupportedSaveFormats :: [SaveFormat]
gnuplotSupportedSaveFormats = [PNG, SVG, EPS, GIF, JPG, PDF]

gnuplotCommand :: Text -> Text -> OutputSpec -> Text
gnuplotCommand cmdargs exe OutputSpec {..} = [st|#{exe} #{cmdargs} -c "#{oScriptPath}"|]

gnuplotAvailable :: PlotM Bool
gnuplotAvailable = do
  mexe <- executable GNUPlot
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      commandSuccess dir [st|"#{exe}" -h|]

gnuplotCapture :: FigureSpec -> FilePath -> Script
gnuplotCapture = prependCapture gnuplotCaptureFragment
  where
    prependCapture f s fp = mconcat [f s fp, "\n", script s]

gnuplotCaptureFragment :: FigureSpec -> FilePath -> Script
gnuplotCaptureFragment FigureSpec {..} fname =
  [st|
set terminal #{terminalString saveFormat}
set output '#{fname}'
|]

-- | Terminal name for supported save formats
terminalString :: SaveFormat -> Text
terminalString PNG = "pngcairo"
terminalString SVG = "svg"
terminalString EPS = "postscript eps"
terminalString GIF = "gif"
terminalString JPG = "jpeg"
terminalString PDF = "pdfcairo"
terminalString fmt = errorWithoutStackTrace $ "gnuplot: unsupported save format" <> show fmt
