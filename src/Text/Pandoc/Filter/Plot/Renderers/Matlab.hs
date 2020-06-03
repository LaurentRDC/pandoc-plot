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

import           Text.Pandoc.Filter.Plot.Renderers.Prelude


matlabSupportedSaveFormats :: [SaveFormat]
matlabSupportedSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]


matlabCommand :: Configuration -> FigureSpec -> FilePath -> IO Text
matlabCommand Configuration{..} _ fp = do
    exe <- tryToFindExe matlabExe
    return [st|#{exe} -batch "run('#{fp}')"|]


-- On Windows at least, "matlab -help"  actually returns -1, even though the
-- help text is shown successfully!
-- Therefore, we cannot rely on this behavior to know if matlab is present, 
-- like other toolkits.
matlabAvailable :: Configuration -> IO Bool
matlabAvailable Configuration{..} = existsOnPath (matlabExe <> ext)
    where
        -- The @which@ function from Turtle only works on
        -- windows if the executable extension is included.
        ext = if isWindows then ".exe" else mempty


matlabCapture :: FigureSpec -> FilePath -> Script
matlabCapture FigureSpec{..} fname = [st|
saveas(gcf, '#{fname}')
|]
