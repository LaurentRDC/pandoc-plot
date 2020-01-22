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
    available <- availableToolkits def
    unavailable <- unavailableToolkits def
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
        , testWithSource
        , testOverrideConfiguration
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
        -- The example config reflects the Windows default
        -- Therefore, we need to test against the Windows default,
        -- even on other OSes
        let config = def { matplotlibExe = "python"
                         , plotlyPythonExe = "python"
                         }

        parsedConfig <- configuration "example-config.yml"
        assertEqual "" config parsedConfig