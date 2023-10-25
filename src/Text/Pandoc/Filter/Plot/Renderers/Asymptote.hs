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
-- Rendering Asymptote plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Asymptote
  ( asymptote,
    asymptoteSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude
import Data.Char(toLower)

asymptote :: PlotM Renderer
asymptote = do
  cmdargs <- asksConfig asyCmdArgs
  return
    $ Renderer
      { rendererToolkit = Asymptote,
        rendererCapture = asymptoteCapture,
        rendererCommand = asymptoteCommand cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -environment|],
        rendererSupportedSaveFormats = asymptoteSupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "asy",
        rendererComment = mappend "// ",
        rendererScriptExtension = ".asy"
      }

asymptoteSupportedSaveFormats :: [SaveFormat]
asymptoteSupportedSaveFormats = [PDF, EPS, PNG]

asymptoteCommand :: Text -> OutputSpec -> Text
asymptoteCommand cmdArgs OutputSpec {..} =
  [st|#{pathToExe oExecutable} #{cmdArgs} -f #{toLower <$> show (saveFormat oFigureSpec)} -o "#{oFigurePath}" "#{oScriptPath}"|]

-- Asymptote export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
asymptoteCapture :: FigureSpec -> FilePath -> Script
asymptoteCapture FigureSpec {..} _ = script
