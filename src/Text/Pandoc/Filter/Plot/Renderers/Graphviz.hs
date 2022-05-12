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
-- Rendering Graphviz plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Graphviz
  ( graphviz,
    graphvizSupportedSaveFormats,
  )
where

import Data.Char
import Text.Pandoc.Filter.Plot.Renderers.Prelude

graphviz :: PlotM Renderer
graphviz = do
      cmdargs <- asksConfig graphvizCmdArgs
      return $
        Renderer
          { rendererToolkit = Graphviz,
            rendererCapture = graphvizCapture,
            rendererCommand = graphvizCommand cmdargs,
            rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -?|],
            rendererSupportedSaveFormats = graphvizSupportedSaveFormats,
            rendererChecks = mempty,
            rendererLanguage = "dot",
            rendererComment = mappend "// ",
            rendererScriptExtension = ".dot"
          }

graphvizSupportedSaveFormats :: [SaveFormat]
graphvizSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, WEBP, GIF]

graphvizCommand :: Text -> OutputSpec -> Text
graphvizCommand cmdargs OutputSpec {..} =
  let fmt = fmap toLower . show . saveFormat $ oFigureSpec
      dpi' = dpi oFigureSpec
   in [st|#{pathToExe oExecutable} #{cmdargs} -T#{fmt} -Gdpi=#{dpi'} -o "#{oFigurePath}" "#{oScriptPath}"|]

-- Graphviz export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
graphvizCapture :: FigureSpec -> FilePath -> Script
graphvizCapture FigureSpec {..} _ = script
