{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE CPP               #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable
-}

module Text.Pandoc.Filter.Plot.Renderers.Matplotlib (matplotlib) where

import           Text.Pandoc.Filter.Plot.Internal
import           Text.Shakespeare.Text           (st)

matplotlib :: Renderer
matplotlib = Renderer {
      rendererName = "matplotlib"
    , rendererSaveFormats = [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    , capture = captureMatplotlib
}


captureMatplotlib :: FigureSpec -> FilePath -> Script
captureMatplotlib FigureSpec{..} fname = [st|
import matplotlib.pyplot as plt
plt.savefig(r"#{fname}", dpi=#{dpi})
|]

