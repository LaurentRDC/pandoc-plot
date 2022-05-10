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
-- Rendering SageMath figures
module Text.Pandoc.Filter.Plot.Renderers.SageMath
  ( sagemath,
    sagemathSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

sagemath :: PlotM (Maybe Renderer)
sagemath = do
  avail <- sagemathAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig sagemathCmdArgs
      return $
          return
            Renderer
              { rendererToolkit = SageMath,
                rendererCapture = sagemathCapture,
                rendererCommand = sagemathCommand cmdargs,
                rendererSupportedSaveFormats = sagemathSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "sagemath",
                rendererComment = mappend "# ",
                rendererScriptExtension = ".sage"
              }

-- See here:
-- https://doc.sagemath.org/html/en/reference/plotting/sage/plot/graphics.html#sage.plot.graphics.Graphics.save
sagemathSupportedSaveFormats :: [SaveFormat]
sagemathSupportedSaveFormats = [EPS, PDF, PNG, SVG]

sagemathCommand :: Text -> OutputSpec -> Text
sagemathCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}"|]

sagemathAvailable :: PlotM Bool
sagemathAvailable = do
  mexe <- executable SageMath
  case mexe of
    Nothing -> return False
    Just (Executable dir exe) -> do
      withPrependedPath dir $ asks envCWD >>= flip commandSuccess [st|#{exe} -v|]


sagemathCapture :: FigureSpec -> FilePath -> Script
sagemathCapture = appendCapture sagemathCaptureFragment


-- This capture fragment is a bit ugly because sage does not have the
-- equivalent of matplotlib's `plt.gca()` to get a pointer to the most
-- recent graphical object. We must search for it
sagemathCaptureFragment :: FigureSpec -> FilePath -> Script
sagemathCaptureFragment FigureSpec {..} fname =
  [st|
import sage.plot.graphics as go
import sage.plot.plot3d.base as go3d
import builtins
# Try to concatenate 3D graphics objects first; if this doesn't work, then 
# concatenate all 2D graphic objects.
__all_graphics = builtins.sum( (obj for obj in globals().values() if isinstance(obj, go3d.Graphics3d)))
if not __all_graphics:
    __all_graphics = builtins.sum( (obj for obj in globals().values() if isinstance(obj, go.Graphics)))
if not __all_graphics:
    raise RuntimeError(''.join([
        "No plotting objects detected. ",
        "Make sure that all of your plotting objects are named, e.g. `G = plot(...)` rather than just `plot(...)`. ",
        "This is a limitation specific to the interaction between sage and pandoc-plot."
    ]))
__all_graphics.save_image(r"#{fname}", dpi=#{dpi})
|]
