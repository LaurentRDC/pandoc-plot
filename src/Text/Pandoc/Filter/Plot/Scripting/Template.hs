{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.Filter.Plot.Scripting.Template (sourceTemplate_) where

import Data.String (fromString)
import Language.Haskell.TH.Syntax
import System.FilePath ((</>))

sourceTemplate_ :: Q Exp
sourceTemplate_ = do
  let fp = ("data" </> "srctemplate.html")
  qAddDependentFile fp
  d <- runIO $ readFile fp
  strToExp d
  where
    strToExp :: String -> Q Exp
    strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)
