{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
This module was inspired by pandoc-crossref
|-}

module ManPage ( embedManualHtml ) where

import           Data.String                (fromString)
import           Data.Text                  (unpack)
import qualified Data.Text.IO               as TIO

import           System.FilePath            ((</>))

import           Language.Haskell.TH.Syntax

docFile :: FilePath
docFile = "docs" </> "MANUAL.html"

readDocFile :: IO String
readDocFile = TIO.readFile docFile >>= return . unpack 

embedManualHtml :: Q Exp
embedManualHtml = do
    qAddDependentFile docFile
    d <- runIO readDocFile
    strToExp d
    where
        strToExp :: String -> Q Exp
        strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)
