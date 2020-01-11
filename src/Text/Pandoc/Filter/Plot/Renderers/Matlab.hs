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

Rendering Matlab code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Matlab (
    MatlabM
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude


newtype MatlabM a 
    = MatlabM { unMatlabM :: ReaderT MatlabConfig IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader MatlabConfig)

instance RendererM MatlabConfig MatlabM where
    run cp ma = do
        config <- loadConfig cp
        runReaderT (unMatlabM ma) config

    name = return "matlabplot"
    scriptExtension = return "m"
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    command _ fp = return [st|matlab -batch -r "try, run(#{fp}), catch, exit, end, exit"|]
    capture = matlabCapture

data MatlabConfig = MatlabConfig
    { matlabBaseConfig :: BaseConfig
    , matlabPreamble :: Script     -- ^ Include script integrated at the beginning of every @matlab@ code block.
    }

instance HasBaseConfig MatlabConfig where
    baseConfig = matlabBaseConfig

instance HasPreamble MatlabConfig where
    ppreamble = matlabPreamble

instance Default MatlabConfig where
    def = MatlabConfig
        { matlabBaseConfig = def
        , matlabPreamble = mempty
        }

instance FromJSON MatlabConfig where
    parseJSON = withObject "plotly" $ \o -> do
        let matlabBaseConfig = (def::BaseConfig)
        matlabPreamble <- o .:? "preamble" .!= mempty
        return MatlabConfig{..}


matlabCapture :: FigureSpec -> FilePath -> MatlabM Script
matlabCapture FigureSpec{..} fname = return [st|
saveas(gcf, '#{fname}')
|]