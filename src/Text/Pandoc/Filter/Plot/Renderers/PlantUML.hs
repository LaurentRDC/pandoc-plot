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
-- Rendering PlantUML markup
module Text.Pandoc.Filter.Plot.Renderers.PlantUML
  ( plantuml,
    plantumlSupportedSaveFormats,
  )
where

import Data.Char
import System.FilePath (takeDirectory, takeFileName, (</>))
import Data.Text (replace, pack)
import Text.Pandoc.Filter.Plot.Renderers.Prelude

plantuml :: PlotM (Maybe Renderer)
plantuml = do
  avail <- plantumlAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig plantumlCmdArgs
      mexe <- executable PlantUML
      return $
        mexe >>= \exe@(Executable _ exename) ->
          return
            Renderer
              { rendererToolkit = PlantUML,
                rendererExe = exe,
                rendererCapture = plantumlCapture,
                rendererCommand = plantumlCommand cmdargs exename,
                rendererSupportedSaveFormats = plantumlSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "plantuml",
                rendererComment = mappend "' ",
                rendererScriptExtension = ".txt"
              }

plantumlSupportedSaveFormats :: [SaveFormat]
plantumlSupportedSaveFormats = [PNG, PDF, SVG]

plantumlCommand :: Text -> Text -> OutputSpec -> Text
plantumlCommand cmdargs exe OutputSpec {..} =
  let fmt = fmap toLower . show . saveFormat $ oFigureSpec
      dir = takeDirectory oFigurePath
   in [st|#{exe} #{cmdargs} -t#{fmt} -output "#{oCWD </> dir}" "#{normalizePath oScriptPath}"|]

normalizePath :: String -> String
normalizePath = map f
    where f '\\' = '/'
          f x = x

plantumlAvailable :: PlotM Bool
plantumlAvailable = do
  mexe <- executable PlantUML
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) -> do
      cmdargs <- asksConfig plantumlCmdArgs
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} #{cmdargs} -h|]

plantumlCapture :: FigureSpec -> FilePath -> Script
plantumlCapture FigureSpec {..} fp = 
    -- Only the filename is included in the script; we need to also pass the ABSOLUTE output directory 
    -- to the executable.
    replace "@startuml" ("@startuml " <> pack (takeFileName fp)) script
