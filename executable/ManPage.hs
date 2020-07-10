{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
This module was inspired by pandoc-crossref
|-}

module ManPage ( embedManualHtml ) where

import qualified Data.Map.Strict            as M

import           Data.String 
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Language.Haskell.TH.Syntax

import qualified Text.DocTemplates          as D
import qualified Text.Pandoc                as P
import           Text.Pandoc.Highlighting   (pygments)

docFile :: FilePath
docFile = "MANUAL.md"

readDocFile :: IO String
readDocFile = TIO.readFile docFile >>= return . T.unpack 

readerOpts :: P.ReaderOptions
readerOpts = P.def { P.readerExtensions = P.githubMarkdownExtensions
                   , P.readerStandalone = True
                   }

writerOpts :: P.WriterOptions
writerOpts = P.def 
    { P.writerHighlightStyle = Just pygments
    , P.writerVariables = D.Context $ M.fromList [("css", D.SimpleVal "https://cdn.jsdelivr.net/npm/bulma@0.9.0/bulma.sass")] 
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
    embedManual $ P.writeHtml5String writerOpts

    -- TODO: something like this:
    -- embedManual $ \doc -> do
    --     template <- P.getTemplate ("docs" </> "default.html") 
    --     template' <- either error id $ P.runWithDefaultPartials . P.compileTemplate mempty
    --     P.writeHtml5String writerOpts { P.writerTemplate = Just template' } doc
