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
-- Rendering Bokeh code blocks
module Text.Pandoc.Filter.Plot.Renderers.Bokeh
  ( bokeh,
    bokehSupportedSaveFormats,
  )
where

import Data.Monoid (Any (..))
import qualified Data.Text as T
import Text.Pandoc.Filter.Plot.Renderers.Prelude

bokeh :: PlotM Renderer
bokeh = do
      cmdargs <- asksConfig bokehCmdArgs
      return $
        Renderer
          { rendererToolkit = Bokeh,
            rendererCapture = appendCapture bokehCaptureFragment,
            rendererCommand = bokehCommand cmdargs,
            rendererAvailability = CommandSuccess $ \exe -> [st|#{pathToExe exe} -c "import bokeh; import selenium"|],
            rendererSupportedSaveFormats = bokehSupportedSaveFormats,
            rendererChecks = [bokehCheckIfShow],
            rendererLanguage = "python",
            rendererComment = mappend "# ",
            rendererScriptExtension = ".py"
          }

bokehSupportedSaveFormats :: [SaveFormat]
bokehSupportedSaveFormats = [PNG, SVG, HTML]

bokehCommand :: Text -> OutputSpec -> Text
bokehCommand cmdargs OutputSpec {..} = [st|#{pathToExe oExecutable} #{cmdargs} "#{oScriptPath}"|]

-- | Check if `bokeh.io.show()` calls are present in the script,
-- which would halt pandoc-plot
bokehCheckIfShow :: Script -> CheckResult
bokehCheckIfShow s =
  if getAny $ mconcat showPresent
    then CheckFailed "encountered a call to `bokeh.io.show`."
    else CheckPassed
  where
    showPresent =
      (\n -> Any (T.isInfixOf n s))
        <$> [ "bokeh.io.show(",
              "show("
            ]

bokehCaptureFragment :: FigureSpec -> FilePath -> Script
bokehCaptureFragment FigureSpec {..} fname =
  [st|
from bokeh.io import export_png, export_svgs, save
from bokeh.models import Model
from bokeh.resources import CDN

# The heuristic to determine the current Model is to find all objects which are
# at least subclasses of bokeh.models.Model, and then find the one which was
# created last. This is a dirty hack, so if you're reading this, don't hesitate to
# suggest something else.
__current_model = [obj for obj in globals().values() if isinstance(obj, Model)][-1]
#{write}
|]
  where
    write = case saveFormat of
      HTML -> [st|save(__current_model, filename=r"#{fname}", resources=CDN)|]
      SVG -> [st|__current_model.output_backend="svg"; export_svgs(__current_model, filename=r"#{fname}")|]
      PNG -> [st|export_png(obj = __current_model, filename=r"#{fname}")|]
      fmt -> errorWithoutStackTrace $ "Save format not supported: " <> show fmt
