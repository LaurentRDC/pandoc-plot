{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Utilities to clean pandoc-plot output directories.
-}

module Text.Pandoc.Filter.Plot.Clean (
      cleanOutputDirs
    , outputDirs
    , readDoc
) where

-- TODO: forConcurrently
import           Control.Monad.Reader             (forM)

import qualified Data.ByteString.Lazy             as B
import           Data.Char                        (toLower)
import           Data.Default                     (def)
import           Data.List                        (nub)
import           Data.Maybe                       (fromMaybe, catMaybes)

import           Data.Text                        (Text, pack)
import qualified Data.Text.IO                     as Text

import           System.Directory                 (removePathForcibly)

import           System.FilePath                  (takeExtension) 

import           Text.Pandoc.Class                (runIO)
import           Text.Pandoc.Definition
import           Text.Pandoc.Error                (handleError)
import qualified Text.Pandoc.Readers              as P
import qualified Text.Pandoc.Options              as P
import           Text.Pandoc.Walk                 (query, Walkable)

import Text.Pandoc.Filter.Plot.Parse
import Text.Pandoc.Filter.Plot.Monad


-- | Clean all output related to pandoc-plot. This includes output directories specified 
-- in the configuration and in the document/block, as well as log files. 
-- Note that *all* files in pandoc-plot output directories will be removed.
--
-- The cleaned directories are returned.
cleanOutputDirs :: Walkable Block b 
                => Configuration -> b -> IO [FilePath]
cleanOutputDirs conf doc = do
    dirs <- runPlotM conf . cleanOutputDirsM $ doc
    -- Deletion of the log file must be done outside of PlotM
    -- to ensure the log file has been closed.
    case logSink conf of 
        LogFile path -> removePathForcibly path
        _            -> return ()
    return dirs


-- | Analyze a document to determine where would the pandoc-plot output directories be.
outputDirs :: Walkable Block b 
           => b -> PlotM [FilePath]
outputDirs = fmap (nub . catMaybes) 
           . sequence 
           . query (\b -> [parseFigureSpec b >>= return . fmap directory])


-- PlotM version of @cleanOutputDirs@
cleanOutputDirsM :: Walkable Block b 
                 => b -> PlotM [FilePath]
cleanOutputDirsM doc = do
    directories <- outputDirs doc
    forM directories $ \fp -> do
        info $ "Removing directory " <> pack fp
        -- It is important to use `removePathForcibly` here, because it does 
        -- not throw exceptions if the directory doesn't exist. This means
        -- we do not have to check in advance if directories are nested in our 
        -- list of directories.
        liftIO $ removePathForcibly fp
        return fp


-- | Read a document, guessing what extensions and reader options are appropriate. If
-- the file cannot be read for any reason, an error is thrown.
readDoc :: FilePath -> IO Pandoc
readDoc fp = handleError =<< (runIO $ do
        let fmt = fromMaybe mempty (formatFromFilePath fp)
        (reader, exts) <- P.getReader fmt
        let readerOpts = def {P.readerExtensions = exts}
        case reader of 
            P.TextReader fct -> do
                t <- liftIO $ Text.readFile fp 
                fct readerOpts t
            P.ByteStringReader bst -> do
                b <- liftIO $ B.readFile fp
                bst readerOpts b)


-- Determine format based on file extension
-- Note : this is exactly the heuristic used by pandoc here:
-- https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/App/FormatHeuristics.hs
--
-- However, this is not exported, so I must re-define it here.
formatFromFilePath :: FilePath -> Maybe Text
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc"     -> Just "asciidoc"
    ".asciidoc" -> Just "asciidoc"
    ".context"  -> Just "context"
    ".ctx"      -> Just "context"
    ".db"       -> Just "docbook"
    ".doc"      -> Just "doc"  -- so we get an "unknown reader" error
    ".docx"     -> Just "docx"
    ".dokuwiki" -> Just "dokuwiki"
    ".epub"     -> Just "epub"
    ".fb2"      -> Just "fb2"
    ".htm"      -> Just "html"
    ".html"     -> Just "html"
    ".icml"     -> Just "icml"
    ".json"     -> Just "json"
    ".latex"    -> Just "latex"
    ".lhs"      -> Just "markdown+lhs"
    ".ltx"      -> Just "latex"
    ".markdown" -> Just "markdown"
    ".md"       -> Just "markdown"
    ".ms"       -> Just "ms"
    ".muse"     -> Just "muse"
    ".native"   -> Just "native"
    ".odt"      -> Just "odt"
    ".opml"     -> Just "opml"
    ".org"      -> Just "org"
    ".pdf"      -> Just "pdf"  -- so we get an "unknown reader" error
    ".pptx"     -> Just "pptx"
    ".roff"     -> Just "ms"
    ".rst"      -> Just "rst"
    ".rtf"      -> Just "rtf"
    ".s5"       -> Just "s5"
    ".t2t"      -> Just "t2t"
    ".tei"      -> Just "tei"
    ".tei.xml"  -> Just "tei"
    ".tex"      -> Just "latex"
    ".texi"     -> Just "texinfo"
    ".texinfo"  -> Just "texinfo"
    ".text"     -> Just "markdown"
    ".textile"  -> Just "textile"
    ".txt"      -> Just "markdown"
    ".wiki"     -> Just "mediawiki"
    ".xhtml"    -> Just "html"
    ".ipynb"    -> Just "ipynb"
    ".csv"      -> Just "csv"
    ['.',y]     | y `elem` ['1'..'9'] -> Just "man"
    _           -> Nothing