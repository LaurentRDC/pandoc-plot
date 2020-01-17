{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                    (forM_)

import           Data.Default.Class               (Default, def)
import           Data.Text                        (Text, unpack)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Common
import           Text.Pandoc.Filter.Plot.Internal

main :: IO ()
main = do
    available <- availableToolkits
    unavailable <- unavailableToolkits
    forM_ unavailable $ \tk -> do
        putStrLn $ show tk <> " is not availble. Its tests will be skipped."

    defaultMain $ testGroup "All tests"
        [ testGroup
            "Configuration tests"
            [ testEmptyConfiguration
            , testExampleConfiguration
            ]        
        , testGroup
            "Toolkit tests"
            (toolkitSuite <$> available)
        ]


-- | Suite of tests that every renderer should pass
toolkitSuite :: Toolkit -> TestTree
toolkitSuite tk =
    testGroup (show tk) $
        [ testFileCreation
        , testFileInclusion
        , testSaveFormat
        ] <*> [tk]


testEmptyConfiguration :: TestTree
testEmptyConfiguration = 
    testCase "empty configuration is correctly parsed to default values" $ do
        let config = def

        parsedConfig <- configuration "tests/fixtures/.pandoc-plot.yml"
        assertEqual "" config parsedConfig


-- The exampel configuration is build by hand (to add comments)
-- and it is embedded into the executable. Therefore, we must make sure it 
-- is correctly parsed (and is therefore valid.)
testExampleConfiguration :: TestTree
testExampleConfiguration = 
    testCase "example configuration is correctly parsed" $ do
        let config = def { matplotlibPreamble   = "matplotlib.py" 
                         , matlabPreamble       = "matlab.m"
                         , plotlyPythonPreamble = "plotly-python.py"
                         , mathematicaPreamble  = "mathematica.m"
                         , octavePreamble       = "octave.m"
                         }

        parsedConfig <- configuration "example-config.yml"
        assertEqual "" config parsedConfig