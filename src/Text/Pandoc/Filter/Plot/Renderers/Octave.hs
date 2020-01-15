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

Rendering Octave plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Octave (
      OctaveM(..)
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude


newtype OctaveM a 
    = OctaveM { unOctaveM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)

instance RendererM OctaveM where
    toolkit = return Octave
    scriptExtension = return ".m"
    comment t = return $ mconcat ["(*", t, "*)"]
    preambleSelector = asks octavePreamble
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    -- It seems that math.exe and wolfram.exe are the same program. What gives?
    command _ fp = return [st|octave --no-window-system #{fp}|]
    capture = octaveCapture


octaveCapture :: FigureSpec -> FilePath -> OctaveM Script
octaveCapture FigureSpec{..} fname = return [st|
saveas(gcf, '#{fname}')
|]