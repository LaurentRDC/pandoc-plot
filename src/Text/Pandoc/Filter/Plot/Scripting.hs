{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ( ScriptResult(..)
    , runTempScript
    , runScriptIfNecessary
    , toImage
    ) where

import           Control.Monad.IO.Class

import           Data.Hashable                        (hash)
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T

import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist)
import           System.Exit                          (ExitCode (..))
import           System.FilePath                      (FilePath, addExtension,
                                                       normalise, takeDirectory,
                                                       replaceExtension, (</>))
import           System.IO.Temp                       (getCanonicalTemporaryDirectory)
import           System.Process.Typed                 (runProcess, shell)

import           Text.Pandoc.Builder                  (fromList, imageWith, link,
                                                       para, toList)
import           Text.Pandoc.Definition               (Block (..))

import           Text.Pandoc.Filter.Plot.Types
import           Text.Pandoc.Filter.Plot.Parse        (captionReader)


-- | Possible result of running a script
data ScriptResult
    = ScriptSuccess
    | ScriptChecksFailed String
    | ScriptFailure Int
        
-- Run script as described by the spec, only if necessary
runScriptIfNecessary :: RendererM m 
                     => FigureSpec -> m ScriptResult
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


-- Run script as described by the spec
-- Checks are performed, according to the renderer
runTempScript :: RendererM m 
              => FigureSpec -> m ScriptResult
runTempScript spec@FigureSpec{..} = do
    checks <- scriptChecks
    let checkResult = mconcat $ checks <*> [script]
    case checkResult of
        CheckFailed msg -> return $ ScriptChecksFailed msg
        CheckPassed -> do
            -- We involve the script hash as a temporary filename
            -- so that there is never any collision
            scriptPath <- tempScriptPath spec
            scriptWithCapture <- do
                captureFragment <- capture spec (figurePath spec)
                return $ mconcat [script, "\n", captureFragment]
            liftIO $ T.writeFile scriptPath scriptWithCapture
            command_ <- T.unpack <$> command spec scriptPath

            ec <- liftIO $ runProcess . shell $ command_
            case ec of
                ExitSuccess      -> return   ScriptSuccess
                ExitFailure code -> return $ ScriptFailure code


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
        withSource'  = withSource spec
        srcLink      = link (T.pack $ replaceExtension target' ".txt") mempty "Source code"
        captionText  = fromList $ fromMaybe mempty (captionReader $ caption spec)
        captionLinks = mconcat [" (", srcLink, ")"]
        caption'     = if withSource' then captionText <> captionLinks else captionText


-- | Determine the temp script path from Figure specifications
-- Note that for certain renderers, the appropriate file extension
-- is important.
tempScriptPath :: RendererM m 
               => FigureSpec -> m FilePath
tempScriptPath FigureSpec{..} = do
    ext <- scriptExtension
    -- Note that matlab will refuse to process files that don't start with
    -- a letter... so we append the renderer name
    let hashedPath = "pandocplot" <> (show . abs . hash $ script) <> ext
    liftIO $ (</> hashedPath) <$> getCanonicalTemporaryDirectory


-- | Determine the path to the source code that generated the figure.
sourceCodePath :: FigureSpec -> FilePath
sourceCodePath = normalise . flip replaceExtension ".txt" . figurePath


-- | Determine the path a figure should have.
figurePath :: FigureSpec -> FilePath
figurePath spec = normalise $ directory spec </> stem spec
  where
    stem = flip addExtension ext . show . hash
    ext  = extension . saveFormat $ spec
