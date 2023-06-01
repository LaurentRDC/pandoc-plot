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
-- Rendering gnuplot plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.GNUPlot
  ( gnuplot,
    gnuplotSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

gnuplot :: PlotM Renderer
gnuplot = do
  cmdargs <- asksConfig gnuplotCmdArgs
  return
    $ Renderer
      { rendererToolkit = GNUPlot,
        rendererCapture = gnuplotCapture,
        rendererCommand = gnuplotCommand cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -h|],
        rendererSupportedSaveFormats = gnuplotSupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "gnuplot",
        rendererComment = mappend "# ",
        rendererScriptExtension = ".gp"
      }

gnuplotSupportedSaveFormats :: [SaveFormat]
gnuplotSupportedSaveFormats = [LaTeX, PNG, SVG, EPS, GIF, JPG, PDF]

gnuplotCommand :: Text -> OutputSpec -> Text
gnuplotCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} -c "#{oScriptPath}"|]

gnuplotCapture :: FigureSpec -> FilePath -> Script
gnuplotCapture = prependCapture gnuplotCaptureFragment
  where
    prependCapture f s fp = mconcat [f s fp, "\n", script s]

gnuplotCaptureFragment :: FigureSpec -> FilePath -> Script
gnuplotCaptureFragment FigureSpec {..} fname =
  [st|
set terminal #{terminalString saveFormat}
set output '#{normalizePath fname}'
|]
  where
    normalizePath = map f
      where
        f '\\' = '/'
        f x = x

-- | Terminal name for supported save formats
terminalString :: SaveFormat -> Text
terminalString PNG = "pngcairo"
terminalString SVG = "svg"
terminalString EPS = "postscript eps"
terminalString GIF = "gif"
terminalString JPG = "jpeg"
terminalString PDF = "pdfcairo"
terminalString LaTeX = "cairolatex"
terminalString fmt = errorWithoutStackTrace $ "gnuplot: unsupported save format" <> show fmt
