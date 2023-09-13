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
-- Rendering GGPlot2 plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.GGPlot2
  ( ggplot2,
    ggplot2SupportedSaveFormats,
  )
where

import qualified Data.Text as T
import Text.Pandoc.Filter.Plot.Renderers.Prelude

ggplot2 :: PlotM Renderer
ggplot2 = do
  cmdargs <- asksConfig ggplot2CmdArgs
  return
    $ Renderer
      { rendererToolkit = GGPlot2,
        rendererCapture = ggplot2Capture,
        rendererCommand = ggplot2Command cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -e "if(!require('ggplot2')) {quit(status=1)}"|],
        rendererSupportedSaveFormats = ggplot2SupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "r",
        rendererComment = mappend "# ",
        rendererScriptExtension = ".r"
      }

ggplot2SupportedSaveFormats :: [SaveFormat]
ggplot2SupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS]

ggplot2Command :: Text -> OutputSpec -> Text
ggplot2Command cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}"|]

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
