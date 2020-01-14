{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Text (unpack)

import Common
import Text.Pandoc.Filter.Plot.Internal

main :: IO ()
main =
    defaultMain $
    testGroup
        "General tests"
        [ generalSuite Matplotlib
        , generalSuite PlotlyPython
        , generalSuite Matlab
        ]

-- | Suite of tests that every renderer should pass
generalSuite :: Toolkit -> TestTree
generalSuite tk = 
    testGroup (show tk) $ 
        [ testFileCreation
        , testFileInclusion
        ] <*> [tk]