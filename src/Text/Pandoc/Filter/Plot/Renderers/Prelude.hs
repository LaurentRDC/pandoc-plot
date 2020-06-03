
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
    , module Text.Pandoc.Filter.Plot.Types
    , Text
    , st
    , unpack
    , commandSuccess
    , existsOnPath
    , tryToFindExe
) where

import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, unpack)

import           System.Directory              (findExecutable)
import           System.Exit                   (ExitCode(..))
import           System.Process.Typed          (runProcess, shell, 
                                                setStdout, setStderr, 
                                                nullStream)
import           Text.Shakespeare.Text         (st)

import qualified Turtle                         as Sh

import           Text.Pandoc.Filter.Plot.Types


-- | Check that the supplied command results in
-- an exit code of 0 (i.e. no errors)
commandSuccess :: Text -> IO Bool
commandSuccess s = do
    ec <- runProcess 
            $ setStdout nullStream 
            $ setStderr nullStream 
            $ shell (unpack s)
    return $ ec == ExitSuccess


-- | Checks that an executable is available on path, at all.
existsOnPath :: FilePath -> IO Bool
existsOnPath fp = Sh.which (Sh.fromString fp) >>= fmap isJust . return


-- | Try to find the executable and normalise its path.
-- If it cannot be found, it is left unchanged - just in case.
tryToFindExe :: String -> IO FilePath
tryToFindExe fp = findExecutable fp >>= maybe (return fp) return