{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Text (unpack)

import Common

main :: IO ()
main =
    defaultMain $
    testGroup
        "General tests"
        [ generalSuite "matplotlib"
        , generalSuite "plotly"
        , generalSuite "matlabplot"
        ]

-- | Suite of tests that every renderer should pass
generalSuite :: RendererName -> TestTree
generalSuite name = 
    testGroup (unpack name) $ 
        [ testFileCreation
        , testFileInclusion
        ] <*> [name]