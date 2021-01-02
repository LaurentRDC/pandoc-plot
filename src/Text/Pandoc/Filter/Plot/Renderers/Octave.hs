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
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = Octave,
                rendererExe = exe,
                rendererCapture = octaveCapture,
                rendererCommand = octaveCommand cmdargs exename,
                rendererSupportedSaveFormats = octaveSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "matlab",
                rendererComment = mappend "% ",
                rendererScriptExtension = ".m"
              }

octaveSupportedSaveFormats :: [SaveFormat]
octaveSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

octaveCommand :: Text -> Text -> OutputSpec -> Text
octaveCommand cmdargs exe OutputSpec {..} = [st|#{exe} #{cmdargs} --no-gui --no-window-system "#{oScriptPath}"|]

octaveAvailable :: PlotM Bool
octaveAvailable = do
  mexe <- executable Octave
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} -h|]

octaveCapture :: FigureSpec -> FilePath -> Script
octaveCapture = appendCapture octaveCaptureFragment

octaveCaptureFragment :: FigureSpec -> FilePath -> Script
octaveCaptureFragment _ fname =
  [st|
saveas(gcf, '#{fname}')
|]
