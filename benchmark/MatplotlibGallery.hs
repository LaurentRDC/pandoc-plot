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

galleryItem1 :: Q Exp
galleryItem1 = galleryItem "benchmark/gallery_item_1.py"

galleryItem2 :: Q Exp
galleryItem2 = galleryItem "benchmark/gallery_item_2.py"