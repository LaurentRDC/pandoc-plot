{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Scripting
-}

module Text.Pandoc.Filter.Plot.Scripting
    ( runTempScript
    , runScriptIfNecessary
    , toImage
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class

import           Data.Hashable                        (hash)
import           Data.List                            (intersperse)
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          (Any (..), (<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T

import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist)
import           System.Exit                          (ExitCode (..))
import           System.FilePath                      (FilePath, addExtension,
                                                       makeValid, normalise, takeDirectory,
                                                       replaceExtension, (</>))
import           System.IO.Temp                       (getCanonicalTemporaryDirectory)
import           System.Process.Typed                 (runProcess, shell)

import           Text.Pandoc.Builder                  (fromList, imageWith, link,
                                                       para, toList)
import           Text.Pandoc.Definition               (Block (..), Inline,
                                                       Pandoc (..))

import           Text.Pandoc.Filter.Plot.Types
import           Text.Pandoc.Filter.Plot.Parse        (captionReader)


-- Run script as described by the spec
runTempScript :: RendererM m => FigureSpec -> m ScriptResult
runTempScript spec@FigureSpec{..} = do
    -- We involve the script hash as a temporary filename
    -- so that there is never any collision
    scriptPath <- tempScriptPath spec
    scriptWithCapture <- do
        captureFragment <- capture spec (figurePath spec)
        return $ mconcat [script, "\n", captureFragment]
    liftIO $ T.writeFile scriptPath scriptWithCapture
    command_ <- T.unpack <$> command spec

    ec <- liftIO $ runProcess . shell $ command_
    case ec of
        ExitSuccess      -> return   ScriptSuccess
        ExitFailure code -> return $ ScriptFailure code

        
-- Run script as described by the spec, only if necessary
runScriptIfNecessary :: RendererM m =>FigureSpec -> m ScriptResult
runScriptIfNecessary spec = do
    liftIO $ createDirectoryIfMissing True . takeDirectory $ figurePath spec

    fileAlreadyExists <- liftIO . doesFileExist $ figurePath spec
    result <- if fileAlreadyExists
                then return ScriptSuccess
                else runTempScript spec

    case result of
        ScriptSuccess      -> liftIO $ T.writeFile (sourceCodePath spec) (script spec) >> return ScriptSuccess
        ScriptFailure code -> return $ ScriptFailure code
        ScriptChecksFailed msg -> return $ ScriptChecksFailed msg


-- | Convert a @FigureSpec@ to a Pandoc block component.
-- Note that the script to generate figure files must still
-- be run in another function.
toImage :: FigureSpec -> Block
toImage spec = head . toList $ para $ imageWith attrs' (T.pack target') "fig:" caption'
    -- To render images as figures with captions, the target title
    -- must be "fig:"
    -- Janky? yes
    where
        attrs'       = blockAttrs spec
        target'      = figurePath spec
        withLinks'   = withLinks spec
        srcLink      = link (T.pack $ replaceExtension target' ".txt") mempty "Source code"
        hiresLink    = link (T.pack $ hiresFigurePath spec) mempty "high res."
        captionText  = fromList $ fromMaybe mempty (captionReader $ caption spec)
        captionLinks = mconcat [" (", srcLink, ", ", hiresLink, ")"]
        caption'     = if withLinks' then captionText <> captionLinks else captionText


-- | Determine the path a figure should have.
figurePath :: FigureSpec -> FilePath
figurePath spec = normalise $ directory spec </> stem spec
  where
    stem = flip addExtension ext . show . hash
    ext  = extension . saveFormat $ spec


tempScriptPath :: RendererM m => FigureSpec -> m FilePath
tempScriptPath FigureSpec{..} = liftIO $ (</> hashedPath) <$> getCanonicalTemporaryDirectory
    where
        hashedPath = show . hash $ script


-- | Determine the path to the source code that generated the figure.
sourceCodePath :: FigureSpec -> FilePath
sourceCodePath = normalise . flip replaceExtension ".txt" . figurePath


-- | The path to the high-resolution figure.
hiresFigurePath :: FigureSpec -> FilePath
hiresFigurePath spec = normalise $ flip replaceExtension (".hires" <> ext) . figurePath $ spec
  where
    ext = extension . saveFormat $ spec