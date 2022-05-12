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
-- Rendering Octave plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Octave
  ( octave,
    octaveSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

octave :: PlotM Renderer
octave = do
      cmdargs <- asksConfig octaveCmdArgs
      return $
        Renderer
          { rendererToolkit = Octave,
            rendererCapture = octaveCapture,
            rendererCommand = octaveCommand cmdargs,
            rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -h|],
            rendererSupportedSaveFormats = octaveSupportedSaveFormats,
            rendererChecks = mempty,
            rendererLanguage = "matlab",
            rendererComment = mappend "% ",
            rendererScriptExtension = ".m"
          }

octaveSupportedSaveFormats :: [SaveFormat]
octaveSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

octaveCommand :: Text -> OutputSpec -> Text
octaveCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} --no-gui --no-window-system "#{oScriptPath}"|]

octaveCapture :: FigureSpec -> FilePath -> Script
octaveCapture = appendCapture octaveCaptureFragment

octaveCaptureFragment :: FigureSpec -> FilePath -> Script
octaveCaptureFragment _ fname =
  [st|
saveas(gcf, '#{fname}')
|]
