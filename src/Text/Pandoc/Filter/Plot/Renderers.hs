{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Specification of renderers.
-}

module Text.Pandoc.Filter.Plot.Renderers where

import           Control.Monad.Reader            (ReaderT, MonadIO, runReaderT)
import           Control.Monad.Reader.Class      (MonadReader)
import           Text.Shakespeare.Text           (st)

import Text.Pandoc.Filter.Plot.Types


runMatplotlib :: Configuration -> MatplotlibM a -> IO a
runMatplotlib config ma = runReaderT (unMatplotlibM ma) config


runPlotly :: Configuration -> PlotlyM a -> IO a
runPlotly config ma = runReaderT (unPlotlyM ma) config


newtype MatplotlibM a 
    = MatplotlibM { unMatplotlibM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)

newtype PlotlyM a 
    = PlotlyM { unPlotlyM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)

newtype MatlabM a
    = MatlabM {unMatlabM :: ReaderT Configuration IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)


instance RendererM MatplotlibM where
    name = return "pyplot"
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    command _ = return mempty

    capture FigureSpec{..} fname = return [st|
import matplotlib.pyplot as plt
plt.savefig(r"#{fname}", dpi=#{dpi})
|]

instance RendererM PlotlyM where
    name = return "plotly"
    supportedSaveFormats = return [PNG, JPG, WEBP, PDF, SVG, EPS]
    command _ = return mempty

    capture _ fname = return [st|
import plotly.graph_objects as go
__current_plotly_figure = next(obj for obj in globals().values() if type(obj) == go.Figure)
__current_plotly_figure.write_image("#{fname}")
|]

instance RendererM MatlabM where
    name = return "matplot"
    supportedSaveFormats = return []
    -- Discussion on matlab scripting here:
    --  https://stackoverflow.com/questions/6657005/matlab-running-an-m-file-from-command-line
    command _ = return $ [st|"matlab -nodisplay -nosplash -nodesktop -r \"try, run(), catch, exit, end, exit\""|]

