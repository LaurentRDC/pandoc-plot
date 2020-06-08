{-# LANGUAGE TemplateHaskellQuotes #-}

module ExampleConfig ( embedExampleConfig ) where


import           Control.DeepSeq            (($!!))

import           Data.String

import           Language.Haskell.TH.Syntax

import System.IO

docFile :: FilePath
docFile = "example-config.yml"

readDocFile :: IO String
readDocFile = withFile docFile ReadMode $ \h -> do
    hSetEncoding h utf8
    cont <- hGetContents h
    return $!! cont

embedExampleConfig :: Q Exp
embedExampleConfig = do
    qAddDependentFile docFile
    s <- runIO readDocFile
    strToExp s
    where
        strToExp :: String -> Q Exp
        strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)