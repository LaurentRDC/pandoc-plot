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
-- Rendering Matlab code blocks
module Text.Pandoc.Filter.Plot.Renderers.Matlab
  ( matlab,
    matlabSupportedSaveFormats,
  )
where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

matlab :: PlotM Renderer
matlab = do
      cmdargs <- asksConfig matlabCmdArgs
      return $
        Renderer
          { rendererToolkit = Matlab,
            rendererCapture = matlabCapture,
            rendererCommand = matlabCommand cmdargs,
            -- On Windows at least, "matlab -help"  actually returns -1, even though the
            -- help text is shown successfully!
            -- Therefore, we cannot rely on this behavior to know if matlab is present,
            -- like other toolkits.
            rendererAvailability = ExecutableExists,
            rendererSupportedSaveFormats = matlabSupportedSaveFormats,
            rendererChecks = mempty,
            rendererLanguage = "matlab",
            rendererComment = mappend "% ",
            rendererScriptExtension = ".m"
          }

matlabSupportedSaveFormats :: [SaveFormat]
matlabSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]

matlabCommand :: Text  -> OutputSpec -> Text
matlabCommand cmdargs OutputSpec {..} = 
  -- The MATLAB 'run' function will switch to the directory where the script
  -- is located before executing the script. Therefore, we first save the current
  -- working directory in the variable 'pandoc_plot_cwd' so that we can use it 
  -- when exporting the figure
  [st|#{pathToExe oExecutable} #{cmdargs} -sd '#{oCWD}' -noFigureWindows -batch "pandoc_plot_cwd=pwd; run('#{oScriptPath}')"|]

matlabCapture :: FigureSpec -> FilePath -> Script
matlabCapture = appendCapture matlabCaptureFragment

matlabCaptureFragment :: FigureSpec -> FilePath -> Script
matlabCaptureFragment FigureSpec {..} fname =
  [st|
if java.io.File('#{fname}').isAbsolute() > 0
  exportpath = '#{fname}';
else
  exportpath = fullfile(pandoc_plot_cwd, '#{fname}');
end

if exist("exportgraphics")>0
    exportgraphics(gcf, exportpath, 'Resolution', #{dpi});
else
    saveas(gcf, exportpath);
end
|]
