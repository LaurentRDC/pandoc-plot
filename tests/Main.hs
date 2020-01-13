{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import Common

main :: IO ()
main =
    defaultMain $
    testGroup
        "Text.Pandoc.Filter.Plot"
        [ testFileCreation
        ]