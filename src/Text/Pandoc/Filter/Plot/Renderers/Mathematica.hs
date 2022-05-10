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
-- Rendering Mathematica plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Mathematica
  ( mathematica,
    mathematicaSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

mathematica :: PlotM (Maybe Renderer)
mathematica = do
  avail <- mathematicaAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig mathematicaCmdArgs
      return $
          return
            Renderer
              { rendererToolkit = Mathematica,
                rendererCapture = mathematicaCapture,
                rendererCommand = mathematicaCommand cmdargs,
                rendererSupportedSaveFormats = mathematicaSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "mathematica",
                rendererComment = \t -> mconcat ["(*", t, "*)"],
                rendererScriptExtension = ".m"
              }

mathematicaSupportedSaveFormats :: [SaveFormat]
mathematicaSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

mathematicaCommand :: Text -> OutputSpec -> Text
mathematicaCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} -script "#{oScriptPath}"|]

mathematicaAvailable :: PlotM Bool
mathematicaAvailable = do
  mexe <- executable Mathematica
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} -h|] -- TODO: test this

mathematicaCapture :: FigureSpec -> FilePath -> Script
mathematicaCapture = appendCapture mathematicaCaptureFragment

mathematicaCaptureFragment :: FigureSpec -> FilePath -> Script
mathematicaCaptureFragment FigureSpec {..} fname =
  [st|
Export["#{fname}", %, #{show saveFormat}, ImageResolution -> #{dpi}]
|]
