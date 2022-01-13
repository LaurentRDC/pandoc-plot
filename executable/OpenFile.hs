-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Open a file in its default program.
module OpenFile (openFile) where

import Data.List (isInfixOf)
import System.Info (os)
import System.Process.Typed (proc, runProcess_, shell)

openFile :: FilePath -> IO ()
openFile
  -- Aliases taken from cabal's Distribution.System module
  | os `elem` ["mingw32", "win32", "cygwin32"] = openFileWindows
  | any (`isInfixOf` os) ["linux", "bsd"] = openFileLinux
  | os `elem` ["darwin"] = openFileMacOS
  | otherwise = error $ "Unsupported OS: " <> os

openFileWindows :: FilePath -> IO ()
openFileWindows fp =
  -- Call looks like: cmd /c 'start "" "my_filepath.html"'
  runProcess_ $ shell $ mconcat ["cmd /c start ", quoted mempty, " ", quoted fp]
  where
    quoted f = mconcat ["\"", f, "\""]

openFileLinux :: FilePath -> IO ()
openFileLinux fp =
  runProcess_ (proc "sh" ["-c", "xdg-open \"$0\" 2>&1 > /dev/null", fp])

openFileMacOS :: FilePath -> IO ()
openFileMacOS fp =
  runProcess_ (proc "open" [fp])
