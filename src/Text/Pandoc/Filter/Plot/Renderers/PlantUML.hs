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
-- Rendering PlantUML markup
module Text.Pandoc.Filter.Plot.Renderers.PlantUML
  ( plantuml,
    plantumlSupportedSaveFormats,
  )
where

import Data.Char
import System.FilePath (takeDirectory, (</>))
import Text.Pandoc.Filter.Plot.Renderers.Prelude

plantuml :: PlotM Renderer
plantuml = do
  cmdargs <- asksConfig plantumlCmdArgs
  return
    $ Renderer
      { rendererToolkit = PlantUML,
        rendererCapture = plantumlCapture,
        rendererCommand = plantumlCommand cmdargs,
        rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} #{cmdargs} -h|],
        rendererSupportedSaveFormats = plantumlSupportedSaveFormats,
        rendererChecks = mempty,
        rendererLanguage = "plantuml",
        rendererComment = mappend "' ",
        rendererScriptExtension = ".txt"
      }

plantumlSupportedSaveFormats :: [SaveFormat]
plantumlSupportedSaveFormats = [PNG, PDF, SVG]

plantumlCommand :: Text -> OutputSpec -> Text
plantumlCommand cmdargs OutputSpec {..} =
  let fmt = fmap toLower . show . saveFormat $ oFigureSpec
      dir = takeDirectory oFigurePath
   in -- the command below works as long as the script name is the same basename
      -- as the target figure path. E.g.: script basename of pandocplot123456789.txt
      -- will result in pandocplot123456789.(extension)
      [st|#{pathToExe oExecutable} #{cmdargs} -t#{fmt} -output "#{oCWD </> dir}" "#{normalizePath oScriptPath}"|]

normalizePath :: String -> String
normalizePath = map f
  where
    f '\\' = '/'
    f x = x

-- PlantUML export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
plantumlCapture :: FigureSpec -> FilePath -> Script
plantumlCapture FigureSpec {..} _ = script
