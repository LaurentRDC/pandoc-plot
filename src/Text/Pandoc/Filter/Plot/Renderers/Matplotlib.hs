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
-- Rendering Matplotlib code blocks.
--
-- Note that the MatplotlibM renderer supports two extra arguments:
--     * @tight_bbox=True|False@ : Make plot bounding box tight. Default is False
--     * @transparent=True|False@ : Make plot background transparent (perfect for web pages). Default is False.
module Text.Pandoc.Filter.Plot.Renderers.Matplotlib
  ( matplotlib,
    matplotlibSupportedSaveFormats,
  )
where

import qualified Data.Map.Strict as M
import Data.Monoid (Any (..))
import qualified Data.Text as T
import Text.Pandoc.Filter.Plot.Renderers.Prelude

matplotlib :: PlotM (Maybe Renderer)
matplotlib = do
  avail <- matplotlibAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig matplotlibCmdArgs
      return $
          return
            Renderer
              { rendererToolkit = Matplotlib,
                rendererCapture = matplotlibCapture,
                rendererCommand = matplotlibCommand cmdargs,
                rendererSupportedSaveFormats = matplotlibSupportedSaveFormats,
                rendererChecks = [matplotlibCheckIfShow],
                rendererLanguage = "python",
                rendererComment = mappend "# ",
                rendererScriptExtension = ".py"
              }

matplotlibSupportedSaveFormats :: [SaveFormat]
matplotlibSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

matplotlibCommand :: Text -> OutputSpec -> Text
matplotlibCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}"|]

matplotlibCapture :: FigureSpec -> FilePath -> Script
matplotlibCapture = appendCapture matplotlibCaptureFragment

matplotlibCaptureFragment :: FigureSpec -> FilePath -> Script
matplotlibCaptureFragment FigureSpec {..} fname =
  [st|
import matplotlib.pyplot as plt
plt.savefig(r"#{fname}", dpi=#{dpi}, transparent=#{transparent}, bbox_inches=#{tightBox})
|]
  where
    attrs = M.fromList extraAttrs
    tight_ = readBool $ M.findWithDefault "False" "tight" attrs
    transparent_ = readBool $ M.findWithDefault "False" "transparent" attrs
    tightBox = if tight_ then ("'tight'" :: Text) else ("None" :: Text)
    transparent = if transparent_ then ("True" :: Text) else ("False" :: Text)

matplotlibAvailable :: PlotM Bool
matplotlibAvailable = do
  mexe <- executable Matplotlib
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) ->
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} -c "import matplotlib"|]

-- | Check if `matplotlib.pyplot.show()` calls are present in the script,
-- which would halt pandoc-plot
matplotlibCheckIfShow :: Script -> CheckResult
matplotlibCheckIfShow s =
  if getAny $ mconcat showPresent
    then CheckFailed "encountered a call to `matplotlib.pyplot.show` or `plt.show`, which would stall `pandoc-plot"
    else CheckPassed
  where
    showPresent =
      (\n -> Any (T.isInfixOf n s))
        <$> [ "matplotlib.pyplot.show()",
              "pyplot.show()",
              "plt.show()"
            ]

-- | Flexible boolean parsing
readBool :: Text -> Bool
readBool s
  | s `elem` ["True", "true", "'True'", "'true'", "1"] = True
  | s `elem` ["False", "false", "'False'", "'false'", "0"] = False
  | otherwise = errorWithoutStackTrace $ unpack $ mconcat ["Could not parse '", s, "' into a boolean. Please use 'True' or 'False'"]
