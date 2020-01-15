{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

import Control.Monad (forM_)

import Data.List        ((\\))
import Data.Text (unpack, Text)

import Test.Tasty
import Test.Tasty.HUnit

import Common
import Text.Pandoc.Filter.Plot.Internal

main :: IO ()
main = do
    available <- availableToolkits
    let missing = toolkits \\ available
    forM_ missing $ \tk -> do
        putStrLn $ show tk <> " is not availble. Its tests will be skipped."

    defaultMain $
        testGroup
            "General tests"
            (generalSuite <$> available)

-- | Suite of tests that every renderer should pass
generalSuite :: Toolkit -> TestTree
generalSuite tk = 
    testGroup (show tk) $ 
        [ testFileCreation
        , testFileInclusion
        ] <*> [tk]
