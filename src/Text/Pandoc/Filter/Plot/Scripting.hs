{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Scripting
module Text.Pandoc.Filter.Plot.Scripting
  ( ScriptResult (..),
    runTempScript,
    runScriptIfNecessary,
    figurePath,
    sourceCodePath,
  )
where

import Control.Exception.Lifted (bracket)
import Control.Monad.Reader
import Data.Default (def)
import Data.Functor.Identity (Identity (..))
import Data.Hashable (hash)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Paths_pandoc_plot (version)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
  )
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath
  ( addExtension,
    normalise,
    replaceExtension,
    takeDirectory,
    (</>),
  )
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Plot.Monad
import Text.Pandoc.Filter.Plot.Scripting.Template
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Templates
import Text.Pandoc.Writers (writeHtml5String)

-- Run script as described by the spec, only if necessary
runScriptIfNecessary :: FigureSpec -> PlotM ScriptResult
runScriptIfNecessary spec = do
  target <- figurePath spec
  liftIO $ createDirectoryIfMissing True . takeDirectory $ target

  fileAlreadyExists <- liftIO . doesFileExist $ target
  result <-
    if fileAlreadyExists
      then return ScriptSuccess
      else runTempScript spec

  logScriptResult result

  case result of
    ScriptSuccess -> writeSource spec >> return ScriptSuccess
    other -> return other
  where
    logScriptResult ScriptSuccess = return ()
    logScriptResult r = err . pack . show $ r

-- | Possible result of running a script
data ScriptResult
  = ScriptSuccess
  | ScriptChecksFailed Text -- Message
  | ScriptFailure Text Int -- Command and exit code

instance Show ScriptResult where
  show ScriptSuccess = "Script success."
  show (ScriptChecksFailed msg) = unpack $ "Script checks failed: " <> msg
  show (ScriptFailure msg ec) = mconcat ["Script failed with exit code ", show ec, " and the following message: ", unpack msg]

-- Run script as described by the spec
-- Checks are performed, according to the renderer
-- Note that stdout from the script is suppressed, but not
-- stderr.
runTempScript :: FigureSpec -> PlotM ScriptResult
runTempScript spec@FigureSpec {..} = do
  let checks = rendererChecks renderer_
      checkResult = mconcat $ checks <*> [script]
  case checkResult of
    CheckFailed msg -> return $ ScriptChecksFailed msg
    CheckPassed -> do
      scriptPath <- tempScriptPath spec
      target <- figurePath spec

      let scriptWithCapture = rendererCapture renderer_ spec target

      liftIO $ T.writeFile scriptPath scriptWithCapture
      let outputSpec =
            OutputSpec
              { oFigureSpec = spec,
                oScriptPath = scriptPath,
                oFigurePath = target
              }
      let command_ = rendererCommand renderer_ outputSpec

      -- Change the PATH environment variable so the appropriate executable is
      -- found first
      let (Executable exedir _) = rendererExe renderer_
      withPrependedPath exedir $ do
        -- It is important that the CWD be inherited from the
        -- parent process. See #2.
        cwd <- asks envCWD
        (ec, _) <- runCommand cwd command_
        case ec of
          ExitSuccess -> return ScriptSuccess
          ExitFailure code -> return $ ScriptFailure command_ code

-- | Determine the temp script path from Figure specifications
-- Note that for certain renderers, the appropriate file extension
-- is important.
tempScriptPath :: FigureSpec -> PlotM FilePath
tempScriptPath FigureSpec {..} = do
  let ext = rendererScriptExtension renderer_
  -- MATLAB will refuse to process files that don't start with
  -- a letter
  -- Note that this hash is only so that we are running scripts from unique
  -- file names; it does NOT determine whether this figure should
  -- be rendered or not.
  let hashedPath = "pandocplot" <> (show . abs . hash $ script) <> ext
  liftIO $ (</> hashedPath) <$> getTemporaryDirectory

-- | Determine the path to the source code that generated the figure.
-- To ensure that the source code path is distinguished from HTML figures, we use the extension .src.html.
sourceCodePath :: FigureSpec -> PlotM FilePath
sourceCodePath = fmap (normalise . flip replaceExtension ".src.html") . figurePath

-- | Hash of the content of a @FigureSpec@. Note that unlike usual hashes,
-- two @FigureSpec@ with the same @figureContentHash@ does not mean that they are equal!
--
-- Not all parts of a FigureSpec are related to running code.
-- For example, changing the caption should not require running the figure again.
figureContentHash :: FigureSpec -> PlotM Word
figureContentHash FigureSpec {..} = do
  dependenciesHash <- sequence $ fileHash <$> dependencies
  -- hash looks strange because instances only exist for 7-tuples or less
  return $
    fromIntegral $
      hash
        ( ( fromEnum (rendererToolkit renderer_),
            script,
            fromEnum saveFormat,
            directory
          ),
          ( dpi,
            dependenciesHash,
            extraAttrs,
            show version -- Included version because capture
          ) -- scripts may change between releases
        )

-- | Determine the path a figure should have.
-- The path for this file is unique to the content of the figure,
-- so that @figurePath@ can be used to determine whether a figure should
-- be rendered again or not.
figurePath :: FigureSpec -> PlotM FilePath
figurePath spec = do
  fh <- figureContentHash spec
  let ext = extension . saveFormat $ spec
      stem = flip addExtension ext . show $ fh
  return $ normalise $ directory spec </> stem

-- | Prepend a directory to the PATH environment variable for the duration
-- of a computation.
--
-- This function is exception-safe; even if an exception happens during the
-- computation, the PATH environment variable will be reverted back to
-- its initial value.
withPrependedPath :: FilePath -> PlotM a -> PlotM a
withPrependedPath dir f = do
  pathVar <- liftIO $ getEnv "PATH"
  let pathVarPrepended = mconcat [dir, ";", pathVar]
  bracket
    (liftIO $ setEnv "PATH" pathVarPrepended)
    (\_ -> liftIO $ setEnv "PATH" pathVar)
    (const f)

-- | Write the source code of a figure to an HTML file with appropriate syntax highlighting.
writeSource :: FigureSpec -> PlotM ()
writeSource spec = do
  let rdr = renderer_ spec
      language = rendererLanguage rdr
  scp <- sourceCodePath spec
  let doc = Pandoc mempty [CodeBlock (mempty, [language], mempty) (script spec)]
      renderSource = \template -> do
        let opts = def {writerTemplate = Just template}
            -- Note that making the document self-contained is absolutely required so that the CSS for
            -- syntax highlighting is included directly in the document.
            t = either (const mempty) id $ runPure (writeHtml5String opts doc >>= makeSelfContained)
        liftIO $ T.writeFile scp t

  either (err . pack) renderSource $ runIdentity $ compileTemplate mempty sourceTemplate

sourceTemplate :: Text
sourceTemplate = pack $(sourceTemplate_)
