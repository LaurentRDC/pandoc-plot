{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable
-}

module Text.Pandoc.Filter.Plot.Renderers.Matplotlib (
      matplotlib
    , MatplotlibConfig(..)
    ) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

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

data MatplotlibConfig = MatplotlibConfig
    { defaultDPI    :: Int  -- ^ The default dots-per-inch value for generated figures. Matplotlib only, ignored otherwise.
    , isTightBbox   :: Bool -- ^ Whether the figures should be saved with @bbox_inches="tight"@ or not. Useful for larger figures with subplots. Matplotlib only, ignored otherwise.
    , isTransparent :: Bool -- ^ If True, figures will be saved with transparent background rather than solid color. .Matplotlib only, ignored otherwise.
    }
    deriving (Eq, Show)
  
instance Default MatplotlibConfig where
    def = MatplotlibConfig
        { defaultDPI    = 80
        , isTightBbox   = False
        , isTransparent = False
        }

dpiKey, isTightBboxKey, isTransparentKey :: IsString a => a
dpiKey           = "dpi"
isTightBboxKey   = "tight_bbox"
isTransparentKey = "transparent"

instance FromJSON MatplotlibConfig where
    parseJSON (Object v) =
        MatplotlibConfig
            <$> v .:? (dpiKey)           .!= (defaultDPI def)
            <*> v .:? (isTightBboxKey)   .!= (isTightBbox def)
            <*> v .:? (isTransparentKey) .!= (isTransparent def)

    parseJSON _ = fail "Could not parse the configuration"

