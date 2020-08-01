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

Rendering Matlab code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Matlab (
      matlabSupportedSaveFormats
    , matlabCommand
    , matlabCapture
    , matlabAvailable
) where

import           System.Directory                            (exeExtension)

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


matlabSupportedSaveFormats :: [SaveFormat]
matlabSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]


matlabCommand :: OutputSpec -> PlotM Text
matlabCommand OutputSpec{..} = do
    exe <- executable Matlab
    return [st|#{exe} -batch "run('#{oScriptPath}')"|]


-- On Windows at least, "matlab -help"  actually returns -1, even though the
-- help text is shown successfully!
-- Therefore, we cannot rely on this behavior to know if matlab is present, 
-- like other toolkits.
matlabAvailable :: PlotM Bool
matlabAvailable = asksConfig matlabExe >>= (\exe -> liftIO $ existsOnPath (exe <> exeExtension))


matlabCapture :: FigureSpec -> FilePath -> Script
matlabCapture FigureSpec{..} fname = [st|
if exist("exportgraphics")>0
    exportgraphics(gcf, '#{fname}', 'Resolution', #{dpi});
else
    saveas(gcf, '#{fname}');
end
|]
