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
import System.FilePath (takeDirectory, (</>))
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
    -- the command below works as long as the script name is the same basename
    -- as the target figure path. E.g.: script basename of pandocplot123456789.txt
    -- will result in pandocplot123456789.(extension)
   in [st|#{exe} #{cmdargs} -t#{fmt} -output "#{oCWD </> dir}" "#{normalizePath oScriptPath}"|]

normalizePath :: String -> String
normalizePath = map f
  where
    f '\\' = '/'
    f x = x

plantumlAvailable :: PlotM Bool
plantumlAvailable = do
  mexe <- executable PlantUML
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) -> do
      cmdargs <- asksConfig plantumlCmdArgs
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} #{cmdargs} -h|]

-- PlantUML export is entirely based on command-line arguments
-- so there is no need to modify the script itself.
plantumlCapture :: FigureSpec -> FilePath -> Script
plantumlCapture FigureSpec {..} _ = script
