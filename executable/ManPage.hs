{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- This module was inspired by pandoc-crossref
-- |
module ManPage (embedManualHtml) where

import Control.Monad (unless)
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import Language.Haskell.TH.Syntax
import System.Directory (doesFileExist)
import System.FilePath ((</>))

docFile :: FilePath
docFile = "docs" </> "MANUAL.html"

readDocFile :: IO String
readDocFile = TIO.readFile docFile >>= return . unpack

embedManualHtml :: Q Exp
embedManualHtml = do
  -- Ensure that the manual file exists
  -- even if it is empty
  manualExists <- runIO $ doesFileExist docFile
  unless manualExists (runIO $ TIO.writeFile docFile mempty)

  qAddDependentFile docFile
  d <- runIO readDocFile
  strToExp d
  where
    strToExp :: String -> Q Exp
    strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)
