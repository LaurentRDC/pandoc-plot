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
-- Rendering Octave plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Octave
  ( octave,
    octaveSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

octave :: PlotM (Maybe Renderer)
octave = do
  avail <- octaveAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig octaveCmdArgs
      mexe <- executable Octave
      return $
        mexe >>= \exe ->
          return
            Renderer
              { rendererToolkit = Octave,
                rendererExe = exe,
                rendererCmdArgs = cmdargs,
                rendererCapture = octaveCapture,
                rendererCommand = octaveCommand,
                rendererSupportedSaveFormats = octaveSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "matlab",
                rendererComment = mappend "% ",
                rendererScriptExtension = ".m"
              }

octaveSupportedSaveFormats :: [SaveFormat]
octaveSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

octaveCommand :: Text -> OutputSpec -> Text -> Text
octaveCommand cmdargs OutputSpec {..} exe = [st|#{exe} #{cmdargs} --no-gui --no-window-system "#{oScriptPath}"|]

octaveAvailable :: PlotM Bool
octaveAvailable = do
  mexe <- executable Octave
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      commandSuccess dir [st|#{exe} -h|]

octaveCapture :: FigureSpec -> FilePath -> Script
octaveCapture = appendCapture octaveCaptureFragment

octaveCaptureFragment :: FigureSpec -> FilePath -> Script
octaveCaptureFragment _ fname =
  [st|
saveas(gcf, '#{fname}')
|]
