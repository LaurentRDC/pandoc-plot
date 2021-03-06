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
      mexe <- executable Mathematica
      return $
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = Mathematica,
                rendererExe = exe,
                rendererCapture = mathematicaCapture,
                rendererCommand = mathematicaCommand cmdargs exename,
                rendererSupportedSaveFormats = mathematicaSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "mathematica",
                rendererComment = \t -> mconcat ["(*", t, "*)"],
                rendererScriptExtension = ".m"
              }

mathematicaSupportedSaveFormats :: [SaveFormat]
mathematicaSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

mathematicaCommand :: Text -> Text -> OutputSpec -> Text
mathematicaCommand cmdargs exe OutputSpec {..} = [st|#{exe} #{cmdargs} -script "#{oScriptPath}"|]

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
