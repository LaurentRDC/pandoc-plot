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
-- Rendering GGPlot2 plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.GGPlot2
  ( ggplot2,
    ggplot2SupportedSaveFormats,
  )
where

import qualified Data.Text as T
import Text.Pandoc.Filter.Plot.Renderers.Prelude

ggplot2 :: PlotM (Maybe Renderer)
ggplot2 = do
  avail <- ggplot2Available
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig ggplot2CmdArgs
      mexe <- executable GGPlot2
      return $
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = GGPlot2,
                rendererExe = exe,
                rendererCapture = ggplot2Capture,
                rendererCommand = ggplot2Command cmdargs exename,
                rendererSupportedSaveFormats = ggplot2SupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "r",
                rendererComment = mappend "# ",
                rendererScriptExtension = ".r"
              }

ggplot2SupportedSaveFormats :: [SaveFormat]
ggplot2SupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, TIF]

ggplot2Command :: Text -> Text -> OutputSpec -> Text
ggplot2Command cmdargs exe OutputSpec {..} = [st|#{exe} #{cmdargs} "#{oScriptPath}"|]

ggplot2Available :: PlotM Bool
ggplot2Available = do
  mexe <- executable GGPlot2
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} -e 'if(!require("ggplot2")) {quit(status=1)}'|]

ggplot2Capture :: FigureSpec -> FilePath -> Script
ggplot2Capture fs fp =
  T.unlines
    [ "pdf(NULL)", -- Prevent the creation of empty Rplots.pdf
      script fs,
      ggplot2CaptureFragment fs fp
    ]

ggplot2CaptureFragment :: FigureSpec -> FilePath -> Script
ggplot2CaptureFragment FigureSpec {..} fname =
  [st|
library(ggplot2) # just in case
ggsave("#{toRPath fname}", plot = last_plot(), dpi = #{dpi})
|]
