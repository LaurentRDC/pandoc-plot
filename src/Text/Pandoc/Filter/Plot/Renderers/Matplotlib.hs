{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Rendering Matplotlib code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Matplotlib (
      runMatplotlib
    , MatplotlibM
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude


runMatplotlib :: Configuration -> MatplotlibM a -> IO a
runMatplotlib config ma = runReaderT (unMatplotlibM ma) config


newtype MatplotlibM a 
    = MatplotlibM { unMatplotlibM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)


instance RendererM MatplotlibM where
    name = return "pyplot"
    scriptExtension = return "py"
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    command _ fp = return [st|python #{fp}|]
    capture = matplotlibCapture


matplotlibCapture :: FigureSpec -> FilePath -> MatplotlibM Script
matplotlibCapture FigureSpec{..} fname = return [st|
import matplotlib.pyplot as plt
plt.savefig(r"#{fname}", dpi=#{dpi})
|]