{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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

Rendering Matplotlib code blocks.

Note that the MatplotlibM renderer supports two extra arguments:
    * @tight_bbox=True|False@ : Make plot bounding box tight. Default is False
    * @transparent=True|False@ : Make plot background transparent (perfect for web pages). Default is False.
-}

module Text.Pandoc.Filter.Plot.Renderers.Matplotlib (
      MatplotlibM(..)
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

import qualified Data.Map.Strict  as M


newtype MatplotlibM a 
    = MatplotlibM { unMatplotlibM :: ReaderT Configuration IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration)

instance RendererM MatplotlibM where
    name = return "matplotlib"
    scriptExtension = return ".py"
    commentChar = return "#"
    preambleSelector = asks matplotlibPreamble
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    parseExtraAttrs = matplotlibExtraAttrs
    command _ fp = return [st|python #{fp}|]
    capture = matplotlibCapture


matplotlibCapture :: FigureSpec -> FilePath -> MatplotlibM Script
matplotlibCapture FigureSpec{..} fname = do
    let attrs        = M.fromList extraAttrs
        tight_       = readBool $ M.findWithDefault "False" "tight"  attrs
        transparent_ = readBool $ M.findWithDefault "False" "transparent" attrs
        tightBox     = if tight_ then ("'tight'"::Text) else ("None"::Text) 
        transparent  = if transparent_ then ("True"::Text) else ("False"::Text)
    return [st|
import matplotlib.pyplot as plt
plt.savefig(r"#{fname}", dpi=#{dpi}, transparent=#{transparent}, bbox_inches=#{tightBox})
|]


matplotlibExtraAttrs :: M.Map Text Text -> MatplotlibM (M.Map Text Text)
matplotlibExtraAttrs kv = return $ M.filterWithKey (\k _ -> k `elem` ["tight_bbox", "transparent"]) kv


-- | Flexible boolean parsing
readBool :: Text -> Bool
readBool s | s `elem` ["True",  "true",  "'True'",  "'true'",  "1"] = True
           | s `elem` ["False", "false", "'False'", "'false'", "0"] = False
           | otherwise = error $ unpack $ mconcat ["Could not parse '", s, "' into a boolean. Please use 'True' or 'False'"]
