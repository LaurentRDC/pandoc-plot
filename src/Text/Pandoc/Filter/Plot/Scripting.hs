{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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

import           Control.Monad.Reader

import           Data.Hashable                     (hash)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, pack, unpack)
import qualified Data.Text.IO                      as T

import           System.Directory                  (createDirectoryIfMissing,
                                                    doesFileExist, getTemporaryDirectory)
import           System.Exit                       (ExitCode (..))
import           System.FilePath                   (addExtension,
                                                    normalise, replaceExtension,
                                                    takeDirectory, (</>))

import           Text.Pandoc.Builder               (fromList, imageWith, link,
                                                    para, toList)
import           Text.Pandoc.Definition            (Block (..), Format)

import           Text.Pandoc.Filter.Plot.Parse     (captionReader)
import           Text.Pandoc.Filter.Plot.Renderers
import           Text.Pandoc.Filter.Plot.Monad


-- Run script as described by the spec, only if necessary
runScriptIfNecessary :: FigureSpec -> PlotM ScriptResult
runScriptIfNecessary spec = do
    liftIO $ createDirectoryIfMissing True . takeDirectory $ figurePath spec

    fileAlreadyExists <- liftIO . doesFileExist $ figurePath spec
    result <- if fileAlreadyExists
                then return ScriptSuccess
                else runTempScript spec

    logScriptResult result

    case result of
        ScriptSuccess -> liftIO $ T.writeFile (sourceCodePath spec) (script spec) >> return ScriptSuccess
        other         -> return other

    where
        logScriptResult ScriptSuccess = return () 
        logScriptResult r             = err   . pack . show $ r


-- | Possible result of running a script
data ScriptResult
    = ScriptSuccess
    | ScriptChecksFailed Text   -- Message
    | ScriptFailure Text Int    -- Command and exit code
    | ToolkitNotInstalled Toolkit -- Script failed because toolkit is not installed

instance Show ScriptResult where
    show ScriptSuccess            = "Script success."
    show (ScriptChecksFailed msg) = unpack $ "Script checks failed: " <> msg
    show (ScriptFailure msg ec)   = mconcat ["Script failed with exit code ", show ec, " and the following message: ", unpack msg]
    show (ToolkitNotInstalled tk) = (show tk) <> " toolkit not installed."


-- Run script as described by the spec
-- Checks are performed, according to the renderer
-- Note that stdout from the script is suppressed, but not
-- stderr.
runTempScript :: FigureSpec -> PlotM ScriptResult
runTempScript spec@FigureSpec{..} = do
    let checks = scriptChecks toolkit
        checkResult = mconcat $ checks <*> [script]
    case checkResult of
        CheckFailed msg -> return $ ScriptChecksFailed msg
        CheckPassed -> do
            scriptPath <- tempScriptPath spec
            let captureFragment = (capture toolkit) spec (figurePath spec)
                -- Note: for gnuplot, the capture string must be placed
                --       BEFORE plotting happens. Since this is only really an
                --       issue for gnuplot, we have a special case.
                scriptWithCapture = if (toolkit == GNUPlot)
                                        then mconcat [captureFragment, "\n", script]
                                        else mconcat [script, "\n", captureFragment]
            liftIO $ T.writeFile scriptPath scriptWithCapture
            let outputSpec = OutputSpec { oFigureSpec = spec
                                        , oScriptPath = scriptPath
                                        , oFigurePath = figurePath spec
                                        }
            command_ <- command toolkit outputSpec
            (ec, _) <- runCommand command_
            case ec of
                ExitSuccess      -> return   ScriptSuccess
                ExitFailure code -> do
                    -- Two possible types of failures: either the script
                    -- failed because the toolkit was not available, or
                    -- because of a genuine error
                    toolkitInstalled <- toolkitAvailable toolkit 
                    if toolkitInstalled
                        then return $ ScriptFailure command_ code
                        else return $ ToolkitNotInstalled toolkit


-- | Convert a @FigureSpec@ to a Pandoc block component.
-- Note that the script to generate figure files must still
-- be run in another function.
toImage :: Format       -- ^ text format of the caption
        -> FigureSpec 
        -> PlotM Block
toImage fmt spec = return 
                 . head 
                 . toList 
                 . para 
                 $ imageWith attrs' (pack target') "fig:" caption'
    -- To render images as figures with captions, the target title
    -- must be "fig:"
    -- Janky? yes
    where
        attrs'       = withInteractiveAttrs $ blockAttrs spec
        target'      = figurePath spec
        withSource'  = withSource spec
        srcLink      = link (pack $ replaceExtension target' ".txt") mempty "Source code"
        captionText  = fromList $ fromMaybe mempty (captionReader fmt $ caption spec)
        captionLinks = mconcat [" (", srcLink, ")"]
        caption'     = if withSource' then captionText <> captionLinks else captionText
        -- for HTML plots, pandoc will replace the <img> tag with an <embed> tag
        -- We include extra attributes with the <embed> tag in mind.
        -- TODO: find way to have HTML "figure" full-width without hardcoding width
        withInteractiveAttrs (a, b, c) = 
            if saveFormat spec == HTML
                then (a, b, c <> [ ("type", "text/html")
                                 , ("style", "width:100%;height:100%;")
                                 ]
                     )
                else (a, b, c)


-- | Determine the temp script path from Figure specifications
-- Note that for certain renderers, the appropriate file extension
-- is important.
tempScriptPath :: FigureSpec -> PlotM FilePath
tempScriptPath FigureSpec{..} = do
    let ext = scriptExtension toolkit
    -- Note that matlab will refuse to process files that don't start with
    -- a letter... so we append the renderer name
    -- Note that this hash is only so that we are running scripts from unique
    -- file names; it does NOT determine whether this figure should
    -- be rendered or not.
    let hashedPath = "pandocplot" <> (show . abs . hash $ script) <> ext
    liftIO $ (</> hashedPath) <$> getTemporaryDirectory


-- | Determine the path to the source code that generated the figure.
sourceCodePath :: FigureSpec -> FilePath
sourceCodePath = normalise . flip replaceExtension ".txt" . figurePath


-- | Determine the path a figure should have.
-- The path for this file is unique to the content of the figure,
-- so that @figurePath@ can be used to determine whether a figure should
-- be rendered again or not.
figurePath :: FigureSpec -> FilePath
figurePath spec = normalise $ directory spec </> stem spec
  where
    stem = flip addExtension ext . show . figureContentHash
    ext  = extension . saveFormat $ spec
