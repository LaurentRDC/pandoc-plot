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
-- Rendering Matlab code blocks
module Text.Pandoc.Filter.Plot.Renderers.Matlab
  ( matlab,
    matlabSupportedSaveFormats,
  )
where

import System.Directory (exeExtension)
import Text.Pandoc.Filter.Plot.Renderers.Prelude

matlab :: PlotM (Maybe Renderer)
matlab = do
  avail <- matlabAvailable
  if not avail
    then return Nothing
    else do
      cmdargs <- asksConfig matlabCmdArgs
      mexe <- executable Matlab
      return $
        mexe >>= \exe ->
          return
            Renderer
              { rendererToolkit = Matlab,
                rendererExe = exe,
                rendererCmdArgs = cmdargs,
                rendererCapture = matlabCapture,
                rendererCommand = matlabCommand,
                rendererSupportedSaveFormats = matlabSupportedSaveFormats,
                rendererChecks = mempty,
                rendererLanguage = "matlab",
                rendererComment = mappend "% ",
                rendererScriptExtension = ".m"
              }

matlabSupportedSaveFormats :: [SaveFormat]
matlabSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

matlabCommand :: Text -> OutputSpec -> Text -> Text
matlabCommand cmdargs OutputSpec {..} exe = [st|#{exe} #{cmdargs} -batch "run('#{oScriptPath}')"|]

-- On Windows at least, "matlab -help"  actually returns -1, even though the
-- help text is shown successfully!
-- Therefore, we cannot rely on this behavior to know if matlab is present,
-- like other toolkits.
matlabAvailable :: PlotM Bool
matlabAvailable = asksConfig matlabExe >>= (\exe -> liftIO $ existsOnPath (exe <> exeExtension))

matlabCapture :: FigureSpec -> FilePath -> Script
matlabCapture = appendCapture matlabCaptureFragment

matlabCaptureFragment :: FigureSpec -> FilePath -> Script
matlabCaptureFragment FigureSpec {..} fname =
  [st|
if exist("exportgraphics")>0
    exportgraphics(gcf, '#{fname}', 'Resolution', #{dpi});
else
    saveas(gcf, '#{fname}');
end
|]
