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

Rendering Matplotlib code blocks.

Note that the MatplotlibM renderer supports two extra arguments:
    * @tight_bbox=True|False@ : Make plot bounding box tight. Default is False
    * @transparent=True|False@ : Make plot background transparent (perfect for web pages). Default is False.
-}

module Text.Pandoc.Filter.Plot.Renderers.Matplotlib (
    MatplotlibM
) where

import Text.Pandoc.Filter.Plot.Renderers.Prelude

import qualified Data.Map.Strict  as M


newtype MatplotlibM a 
    = MatplotlibM { unMatplotlibM :: ReaderT MatplotlibConfig IO a } 
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader MatplotlibConfig)

instance RendererM MatplotlibConfig MatplotlibM where
    run cp ma = do
        config <- loadConfig cp 
        runReaderT (unMatplotlibM ma) config

    name = return "matplotlib"
    scriptExtension = return "py"
    supportedSaveFormats = return [PNG, PDF, SVG, JPG, EPS, GIF, TIF]
    parseExtraAttrs = matplotlibExtraAttrs
    command _ fp = return [st|python #{fp}|]
    capture = matplotlibCapture

data MatplotlibConfig = MatplotlibConfig 
    { matplotlibBaseConfig :: BaseConfig
    , isTightBbox          :: Bool   -- ^ Whether the figures should be saved with @bbox_inches="tight"@ or not. Useful for larger figures with subplots.
    , isTransparent        :: Bool   -- ^ If True, figures will be saved with transparent background rather than solid color.
    , matplotlibPreamble   :: Script -- ^ Include script integrated at the beginning of every @matplotlib@ code block.
    }

instance HasBaseConfig MatplotlibConfig where
    baseConfig = matplotlibBaseConfig

instance HasPreamble MatplotlibConfig where
    ppreamble = matplotlibPreamble

instance Default MatplotlibConfig where
    def = MatplotlibConfig
        { matplotlibBaseConfig    = def
        , isTightBbox             = False
        , isTransparent           = False
        , matplotlibPreamble      = mempty
        }

instance FromJSON MatplotlibConfig where
    parseJSON = withObject "matplotlib" $ \v -> do
        let matplotlibBaseConfig = def
        isTightBox         <- v .:? "tight" .!= False
        isTransparent      <- v .:? "transparent" .!= False
        matplotlibPreamble <- v .:? "preamble" .!= mempty
        return MatplotlibConfig{..}


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
