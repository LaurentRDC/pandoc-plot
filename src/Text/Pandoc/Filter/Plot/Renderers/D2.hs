{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Sanchayan Maity, 2023 - present
-- License     : GNU GPL, version 2 or above
-- Maintainer  : sanchayan@sanchayanmaity.net
-- Stability   : internal
-- Portability : portable
--
-- Rendering D2 plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.D2
  ( d2,
    d2SupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

d2 :: PlotM Renderer
d2 = do
  cmdargs <- asksConfig d2CmdArgs
  return
    $ Renderer
      { rendererToolkit = D2,
        rendererCapture = d2Capture,
        rendererCommand = d2Command cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -v|],
        rendererSupportedSaveFormats = d2SupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "d2",
        rendererComment = mappend "# ",
        rendererScriptExtension = ".d2"
      }

d2SupportedSaveFormats :: [SaveFormat]
d2SupportedSaveFormats = [PNG, PDF, SVG]

d2Command :: Text -> OutputSpec -> Text
d2Command cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}" "#{oFigurePath}"|]

-- d2 export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
d2Capture :: FigureSpec -> FilePath -> Script
d2Capture FigureSpec {..} _ = script
