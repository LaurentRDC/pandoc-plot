{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable
-}

module Text.Pandoc.Filter.Plot.Renderers.Matplotlib (
      matplotlib
    ) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

matplotlib :: Renderer
matplotlib = Renderer {
      rendererName = "matplotlib"
    , rendererSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    , allowedInclusionKeys = mempty
    , capture = captureMatplotlib
}


captureMatplotlib :: FigureSpec -> FilePath -> Script
captureMatplotlib FigureSpec{..} fname = [st|
import matplotlib.pyplot as plt
plt.savefig(r"#{fname}", dpi=#{dpi})
|]
