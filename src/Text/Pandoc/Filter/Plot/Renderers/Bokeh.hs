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

Rendering Bokeh code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Bokeh (
      bokehSupportedSaveFormats
    , bokehCommand
    , bokehCapture
    , bokehAvailable
    , bokehCheckIfShow
) where

import           Data.Monoid                               (Any(..))
import qualified Data.Text                                 as T
import           Text.Pandoc.Filter.Plot.Renderers.Prelude


bokehSupportedSaveFormats :: [SaveFormat]
bokehSupportedSaveFormats = [PNG, HTML]


bokehCommand :: OutputSpec -> PlotM Text
bokehCommand OutputSpec{..} = do
    exe <- executable Bokeh
    return [st|#{exe} "#{oScriptPath}"|]


bokehAvailable :: PlotM Bool
bokehAvailable = do
    exe <- executable Bokeh
    commandSuccess [st|#{exe} -c "import bokeh; import selenium"|]


-- | Check if `matplotlib.pyplot.show()` calls are present in the script,
-- which would halt pandoc-plot
bokehCheckIfShow :: Script -> CheckResult
bokehCheckIfShow s = 
    if getAny $ mconcat showPresent
        then CheckFailed "encountered a call to `bokeh.io.show`."
        else CheckPassed
    where
        showPresent = (\n -> Any (T.isInfixOf n s)) <$> [
                  "bokeh.io.show("
                , "show("
            ]


-- TODO: should we capture a Document instead of a Plot?
--       For some reason, saving the current document was not working
bokehCapture :: FigureSpec -> FilePath -> Script
bokehCapture FigureSpec{..} fname = [st|
from bokeh.io import export_png, export_svgs, save
from bokeh.models import Plot
from bokeh.resources import CDN
__current_plot = next(obj for obj in globals().values() if isinstance(obj, Plot))
#{write}
|]
    where  
        write = case saveFormat of
            HTML -> [st|save(__current_plot, filename=r"#{fname}", resources=CDN)|]
            PNG  -> [st|export_png(obj = __current_plot, filename=r"#{fname}")|]
            fmt  -> error $ "Save format not supported: " <> show fmt
