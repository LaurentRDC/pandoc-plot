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
    , Executable(..)
    , appendCapture
    , toRPath
) where

import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, unpack, pack)

import           System.Directory              (findExecutable)
import           System.FilePath               (splitFileName, isPathSeparator)
import           System.Exit                   (ExitCode(..))

import           Text.Shakespeare.Text         (st)

import           Text.Pandoc.Filter.Plot.Monad

data Executable = Executable FilePath Text


exeFromPath :: FilePath -> Executable
exeFromPath fp = let (dir, name) = splitFileName fp
                 in Executable dir (pack name)

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
tryToFindExe :: String -> IO (Maybe Executable)
tryToFindExe fp = findExecutable fp >>= return . fmap exeFromPath


-- | Path to (directory, executable) of a toolkit. 
executable :: Toolkit -> PlotM (Maybe Executable)
executable Matplotlib   = asksConfig matplotlibExe   >>= liftIO . tryToFindExe 
executable PlotlyPython = asksConfig plotlyPythonExe >>= liftIO . tryToFindExe 
executable PlotlyR      = asksConfig plotlyRExe      >>= liftIO . tryToFindExe 
executable Matlab       = asksConfig matlabExe       >>= liftIO . tryToFindExe 
executable Mathematica  = asksConfig mathematicaExe  >>= liftIO . tryToFindExe 
executable Octave       = asksConfig octaveExe       >>= liftIO . tryToFindExe 
executable GGPlot2      = asksConfig ggplot2Exe      >>= liftIO . tryToFindExe 
executable GNUPlot      = asksConfig gnuplotExe      >>= liftIO . tryToFindExe 
executable Graphviz     = asksConfig graphvizExe     >>= liftIO . tryToFindExe 
executable Bokeh        = asksConfig bokehExe        >>= liftIO . tryToFindExe  
executable Plotsjl      = asksConfig plotsjlExe      >>= liftIO . tryToFindExe 


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


-- | R paths use the '/' path separator
toRPath :: FilePath -> FilePath
toRPath = fmap (\c -> if isPathSeparator c then '/' else c)