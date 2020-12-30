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
-- Rendering Graphviz plots code blocks
module Text.Pandoc.Filter.Plot.Renderers.Graphviz
  ( graphviz,
    graphvizSupportedSaveFormats,
  )
where

import Data.Char
import Text.Pandoc.Filter.Plot.Renderers.Prelude

graphviz :: PlotM (Maybe Renderer)
graphviz = do
  avail <- graphvizAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig graphvizCmdArgs
      mexe <- executable Graphviz
      return $
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = Graphviz,
                rendererExe = exe,
                rendererCapture = graphvizCapture,
                rendererCommand = graphvizCommand cmdargs exename,
                rendererSupportedSaveFormats = graphvizSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "dot",
                rendererComment = mappend "// ",
                rendererScriptExtension = ".dot"
              }

graphvizSupportedSaveFormats :: [SaveFormat]
graphvizSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, WEBP, GIF]

graphvizCommand :: Text -> Text -> OutputSpec -> Text
graphvizCommand cmdargs exe OutputSpec {..} =
  let fmt = fmap toLower . show . saveFormat $ oFigureSpec
      dpi' = dpi oFigureSpec
   in [st|#{exe} #{cmdargs} -T#{fmt} -Gdpi=#{dpi'} -o "#{oFigurePath}" "#{oScriptPath}"|]

graphvizAvailable :: PlotM Bool
graphvizAvailable = do
  mexe <- executable Graphviz
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      commandSuccess dir [st|#{exe} -?|]

-- Graphviz export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
graphvizCapture :: FigureSpec -> FilePath -> Script
graphvizCapture FigureSpec {..} _ = script
