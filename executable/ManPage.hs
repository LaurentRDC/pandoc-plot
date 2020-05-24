{-# LANGUAGE TemplateHaskell #-}
{-|
This module was inspired by pandoc-crossref
|-}

module ManPage ( embedManualHtml ) where

import           Control.DeepSeq            (($!!))

import           Data.String 
import qualified Data.Text                  as T

import           Language.Haskell.TH.Syntax

import qualified Text.Pandoc                as P
import           Text.Pandoc.Highlighting   (pygments)

import System.FilePath                      (FilePath)
import System.IO

docFile :: FilePath
docFile = "README.md"

readDocFile :: IO String
readDocFile = withFile docFile ReadMode $ \h -> do
    hSetEncoding h utf8
    cont <- hGetContents h
    return $!! cont

readerOpts :: P.ReaderOptions
readerOpts = P.def { P.readerExtensions = P.githubMarkdownExtensions
                   , P.readerStandalone = True
                   }

embedManual :: (P.Pandoc -> P.PandocPure T.Text) -> Q Exp
embedManual fmt = do
    qAddDependentFile docFile
    d <- runIO readDocFile
    let pd  = either (error . show) id $ P.runPure $ P.readMarkdown readerOpts (T.pack d)
        txt = either (error . show) id $ P.runPure $ fmt pd
    strToExp $ T.unpack txt
    where
        strToExp :: String -> Q Exp
        strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)

embedManualHtml :: Q Exp
embedManualHtml = do
    embedManual $ P.writeHtml5String P.def { P.writerHighlightStyle = Just pygments }
