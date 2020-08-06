
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
) where

import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, unpack)

import           System.Directory              (findExecutable, canonicalizePath)
import           System.Exit                   (ExitCode(..))

import           Text.Shakespeare.Text         (st)

import           Text.Pandoc.Filter.Plot.Monad


-- | Check that the supplied command results in
-- an exit code of 0 (i.e. no errors)
commandSuccess :: Text -> PlotM Bool
commandSuccess s = do
    (ec, _) <- runCommand s
    return $ ec == ExitSuccess


-- | Checks that an executable is available on path, at all.
existsOnPath :: FilePath -> IO Bool
existsOnPath fp = canonicalizePath fp 
                    >>= findExecutable 
                    >>= fmap isJust . return


-- | Try to find the executable and normalise its path.
-- If it cannot be found, it is left unchanged - just in case.
tryToFindExe :: String -> IO FilePath
tryToFindExe fp = findExecutable fp >>= maybe (return fp) return


-- | Path to the executable of a toolkit. If the executable can
-- be found, then it will be the full path to it.
executable :: Toolkit -> PlotM FilePath
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
executable Plotsjl   = asksConfig plotsjlExe   >>= liftIO . tryToFindExe


-- | Internal description of all information 
-- needed to output a figure.
data OutputSpec = OutputSpec 
    { oFigureSpec    :: FigureSpec    -- ^ Figure spec
    , oScriptPath    :: FilePath      -- ^ Path to the script to render
    , oFigurePath    :: FilePath      -- ^ Figure output path
    } 
