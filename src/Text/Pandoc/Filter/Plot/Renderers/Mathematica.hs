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

Rendering Mathematica plots code blocks
-}

module Text.Pandoc.Filter.Plot.Renderers.Mathematica (
      MathematicaM(..)
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude


newtype MathematicaM a 
    = MathematicaM { unMathematicaM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)

instance RendererM MathematicaM where
    toolkit = return Mathematica
    scriptExtension = return ".m"
    comment t = return $ mconcat ["(*", t, "*)"]
    preambleSelector = asks mathematicaPreamble
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    -- It seems that math.exe and wolfram.exe are the same program. What gives?
    command _ fp = return [st|math -script #{fp}|]
    capture = mathematicaCapture


mathematicaCapture :: FigureSpec -> FilePath -> MathematicaM Script
mathematicaCapture FigureSpec{..} fname = return [st|
Export["#{fname}", %, show saveFormat]
|]