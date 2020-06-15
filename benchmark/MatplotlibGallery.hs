{-# LANGUAGE TemplateHaskell #-}

module MatplotlibGallery where

import           Data.String                (fromString)

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Language.Haskell.TH.Syntax


galleryItem :: FilePath -> Q Exp
galleryItem fp = do
    qAddDependentFile fp
    txt <- runIO $ T.readFile fp
    strToExp $ T.unpack txt
    where
        strToExp :: String -> Q Exp
        strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)

galleryItem1, galleryItem2, galleryItem3, galleryItem4 :: Q Exp
galleryItem1 = galleryItem "benchmark/gallery_item_1.py"
galleryItem2 = galleryItem "benchmark/gallery_item_2.py"
galleryItem3 = galleryItem "benchmark/gallery_item_3.py"
galleryItem4 = galleryItem "benchmark/gallery_item_4.py"