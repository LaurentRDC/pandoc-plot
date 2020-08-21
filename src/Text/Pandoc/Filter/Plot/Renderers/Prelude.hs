{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Prelude for renderers, containing some helpful utilities.
-}
module Text.Pandoc.Filter.Plot.Renderers.Prelude (

      module Prelude
    , module Text.Pandoc.Filter.Plot.Monad
    , Text
    , st
    , unpack
    , commandSuccess
    , existsOnPath
    , executable
    , OutputSpec(..)
    , appendCapture
) where

import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, unpack)

import           System.Directory              (findExecutable)
import           System.FilePath               (splitFileName)
import           System.Exit                   (ExitCode(..))

import           Text.Shakespeare.Text         (st)

import           Text.Pandoc.Filter.Plot.Monad


-- | Check that the supplied command results in
-- an exit code of 0 (i.e. no errors)
commandSuccess :: FilePath -- Directory from which to run the command
               -> Text     -- Command to run, including the executable
               -> PlotM Bool
commandSuccess fp s = do
    (ec, _) <- runCommand fp s
    return $ ec == ExitSuccess


-- | Checks that an executable is available on path, at all.
existsOnPath :: FilePath -> IO Bool
existsOnPath fp = findExecutable fp >>= fmap isJust . return


-- | Try to find the executable and normalise its path.
-- If it cannot be found, it is left unchanged - just in case.
tryToFindExe :: String -> IO FilePath
tryToFindExe fp = findExecutable fp >>= maybe (return fp) return


-- | Path to (directory, executable) of a toolkit. 
executable :: Toolkit -> PlotM (FilePath, FilePath)
executable Matplotlib   = asksConfig matplotlibExe   >>= liftIO . tryToFindExe >>= return . splitFileName
executable PlotlyPython = asksConfig plotlyPythonExe >>= liftIO . tryToFindExe >>= return . splitFileName
executable PlotlyR      = asksConfig plotlyRExe      >>= liftIO . tryToFindExe >>= return . splitFileName
executable Matlab       = asksConfig matlabExe       >>= liftIO . tryToFindExe >>= return . splitFileName
executable Mathematica  = asksConfig mathematicaExe  >>= liftIO . tryToFindExe >>= return . splitFileName
executable Octave       = asksConfig octaveExe       >>= liftIO . tryToFindExe >>= return . splitFileName
executable GGPlot2      = asksConfig ggplot2Exe      >>= liftIO . tryToFindExe >>= return . splitFileName
executable GNUPlot      = asksConfig gnuplotExe      >>= liftIO . tryToFindExe >>= return . splitFileName
executable Graphviz     = asksConfig graphvizExe     >>= liftIO . tryToFindExe >>= return . splitFileName
executable Bokeh        = asksConfig bokehExe        >>= liftIO . tryToFindExe >>= return . splitFileName 
executable Plotsjl      = asksConfig plotsjlExe      >>= liftIO . tryToFindExe >>= return . splitFileName


-- | A shortcut to append capture script fragments to scripts
appendCapture :: (FigureSpec -> FilePath -> Script) 
              ->  FigureSpec -> FilePath -> Script
appendCapture f s fp = mconcat [script s, "\n", f s fp]


-- | Internal description of all information 
-- needed to output a figure.
data OutputSpec = OutputSpec 
    { oFigureSpec    :: FigureSpec    -- ^ Figure spec
    , oScriptPath    :: FilePath      -- ^ Path to the script to render
    , oFigurePath    :: FilePath      -- ^ Figure output path
    } 
