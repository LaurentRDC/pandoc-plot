{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types in use in pandoc-plot
-}

module Text.Pandoc.Filter.Plot.Types where

import           Control.Monad.Reader

import           Data.Char              (toLower)
import           Data.Default.Class     (Default, def)
import           Data.Hashable          (Hashable(..))
import           Data.Semigroup         as Sem
import           Data.Text              (Text, pack)

import           GHC.Generics           (Generic)

import           Text.Pandoc.Definition (Attr)


-- | Monad in which to run pandoc-plot computations
type PlotM a = ReaderT () IO a


type Script = Text


-- | Datatype containing all parameters required to run pandoc-plot.
--
-- It is assumed that once a @FigureSpec@ has been created, no configuration
-- can overload it; hence, a @FigureSpec@ completely encodes a particular figure.
data FigureSpec = FigureSpec
    { caption        :: String     -- ^ Figure caption.
    , withLinks      :: Bool       -- ^ Append links to source code and high-dpi figure to caption.
    , script         :: Script     -- ^ Source code for the figure.
    , saveFormat     :: SaveFormat -- ^ Save format of the figure.
    , directory      :: FilePath   -- ^ Directory where to save the file.
    , dpi            :: Int        -- ^ Dots-per-inch of figure.
    , figureRenderer :: Renderer   -- ^ Rendering library.
    , blockAttrs     :: Attr       -- ^ Attributes not related to @pandoc-plot@ will be propagated.
    } deriving Generic

instance Hashable FigureSpec -- From Generic


data Renderer = Renderer 
    { rendererName :: Text
    , rendererSaveFormats :: [SaveFormat]
    , capture :: FigureSpec -> FilePath -> Script
    }

instance Hashable Renderer where
    hashWithSalt s = hashWithSalt s . rendererName

-- | Generated figure file format supported by pandoc-plot.
-- Note: all formats are supported by Matplotlib, but not all
-- formats are supported by Plotly
data SaveFormat
    = PNG
    | PDF
    | SVG
    | JPG
    | EPS
    | GIF
    | TIF
    | WEBP
    deriving (Bounded, Enum, Eq, Show, Generic)

instance Hashable SaveFormat -- From Generic

-- | Parse an image save format string
--
-- >>> saveFormatFromString ".png"
-- Just PNG
--
-- >>> saveFormatFromString "jpeg"
-- Just JPEG
--
-- >>> SaveFormatFromString "arbitrary"
-- Nothing
saveFormatFromString :: String -> Maybe SaveFormat
saveFormatFromString s
    | s `elem` ["png", "PNG", ".png"] = Just PNG
    | s `elem` ["pdf", "PDF", ".pdf"] = Just PDF
    | s `elem` ["svg", "SVG", ".svg"] = Just SVG
    | s `elem` ["eps", "EPS", ".eps"] = Just EPS
    | s `elem` ["gif", "GIF", ".gif"] = Just GIF
    | s `elem` ["jpg", "jpeg", "JPG", "JPEG", ".jpg", ".jpeg"] = Just JPG
    | s `elem` ["tif", "tiff", "TIF", "TIFF", ".tif", ".tiff"] = Just TIF
    | s `elem` ["webp", ".webp", "WebP", "WEBP"] = Just WEBP
    | otherwise = Nothing

-- | Save format file extension
extension :: SaveFormat -> String
extension fmt = mconcat [".", fmap toLower . show $ fmt]