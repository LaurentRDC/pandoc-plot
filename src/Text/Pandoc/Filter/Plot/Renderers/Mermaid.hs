{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Sanchayan Maity, 2024 - present
-- License     : GNU GPL, version 2 or above
-- Maintainer  : sanchayan@sanchayanmaity.net
-- Stability   : internal
-- Portability : portable
--
-- Rendering Mermaid plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Mermaid
  ( mermaid,
    mermaidSupportedSaveFormats,
  )
where

import Data.Char (toLower)
import Text.Pandoc.Filter.Plot.Renderers.Prelude

mermaid :: PlotM Renderer
mermaid = do
  cmdargs <- asksConfig mermaidCmdArgs
  return
    $ Renderer
      { rendererToolkit = Mermaid,
        rendererCapture = mermaidCapture,
        rendererCommand = mermaidCommand cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -V|],
        rendererSupportedSaveFormats = mermaidSupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "mermaid",
        rendererComment = mempty,
        rendererScriptExtension = ".mermaid"
      }

mermaidSupportedSaveFormats :: [SaveFormat]
mermaidSupportedSaveFormats = [PDF, PNG, SVG]

mermaidCommand :: Text -> OutputSpec -> Text
mermaidCommand cmdargs OutputSpec {..} =
  let fmt = fmap toLower . show . saveFormat $ oFigureSpec
   in [st|#{pathToExe oExecutable} #{cmdargs} -q -e #{fmt} -i "#{oScriptPath}" -o "#{oFigurePath}"|]

-- Mermaid export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
mermaidCapture :: FigureSpec -> FilePath -> Script
mermaidCapture FigureSpec {..} _ = script
