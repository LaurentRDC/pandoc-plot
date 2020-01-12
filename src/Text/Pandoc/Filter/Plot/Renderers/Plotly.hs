{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Rendering Plotly code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Plotly (
      PlotlyM(..)
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude


newtype PlotlyM a 
    = PlotlyM { unPlotlyM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)


instance RendererM PlotlyM where
    name = return "plotly"
    scriptExtension = return "py"
    preambleSelector = asks plotlyPreamble
    supportedSaveFormats = return [PNG, JPG, WEBP, PDF, SVG, EPS]
    command _ fp = return [st|python #{fp}|]
    capture = plotlyCapture
        
        
plotlyCapture :: FigureSpec -> FilePath -> PlotlyM Script
plotlyCapture _ fname = return [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.write_image("#{fname}")
|]