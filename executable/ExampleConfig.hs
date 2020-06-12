{-# LANGUAGE TemplateHaskellQuotes #-}

module ExampleConfig ( embedExampleConfig ) where


import           Data.String
import           Data.Text                  (unpack)
import qualified Data.Text.IO               as TIO

import           Language.Haskell.TH.Syntax

docFile :: FilePath
docFile = "example-config.yml"

readDocFile :: IO String
readDocFile = TIO.readFile docFile >>= return . unpack

embedExampleConfig :: Q Exp
embedExampleConfig = do
    qAddDependentFile docFile
    s <- runIO readDocFile
    strToExp s
    where
        strToExp :: String -> Q Exp
        strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)