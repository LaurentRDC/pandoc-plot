{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Rendering Graphviz plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Graphviz (
      graphvizSupportedSaveFormats
    , graphvizCommand
    , graphvizCapture
    , graphvizAvailable
) where

import           Data.Char
import           Text.Pandoc.Filter.Plot.Renderers.Prelude


graphvizSupportedSaveFormats :: [SaveFormat]
graphvizSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, WEBP, GIF]


graphvizCommand :: OutputSpec -> Text -> Text
graphvizCommand OutputSpec{..} exe = 
    let fmt = fmap toLower . show . saveFormat $ oFigureSpec
        dpi' = dpi oFigureSpec
    in [st|#{exe} -T#{fmt} -Gdpi=#{dpi'} -o "#{oFigurePath}" "#{oScriptPath}"|]


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
graphvizCapture FigureSpec{..} _ = script
