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
      MatlabM(..)
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude


newtype MatlabM a 
    = MatlabM { unMatlabM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)

instance RendererM MatlabM where
    toolkit = return Matlab
    scriptExtension = return ".m"
    commentChar = return "%"
    preambleSelector = asks matlabPreamble
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    command _ fp = return [st|matlab -batch "run('#{fp}')"|]
    capture = matlabCapture


matlabCapture :: FigureSpec -> FilePath -> MatlabM Script
matlabCapture FigureSpec{..} fname = return [st|
saveas(gcf, '#{fname}')
|]