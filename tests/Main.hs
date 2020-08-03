{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                    (forM_)

import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text, unpack)
import qualified Data.Text                        as T

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Text.Pandoc.Builder              as B
import qualified Text.Pandoc.Definition           as B
import           Text.Pandoc.JSON

import           Common
import           Text.Pandoc.Filter.Plot

main :: IO ()
main = do
    available <- availableToolkits defaultTestConfig
    unavailable <- unavailableToolkits defaultTestConfig
    forM_ unavailable $ \tk -> do
        putStrLn $ show tk <> " is not available. Its tests will be skipped."

    defaultMain $ testGroup "All tests"
        [ testGroup
            "Configuration tests"
            [ testEmptyConfiguration
            , testExampleConfiguration
            , testConfigurationPathMeta
            ]        
        , testGroup
            "Parsing tests"
            [ testCaptionReader ]
        , testGroup
            "HTML embedding tests"
            [ testHtmlBodyEmbedding
            , testHtmlEmbedding 
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
        , testMarkdownFormattingCaption1
        , testMarkdownFormattingCaption2
        , testCleanOutputDirs
        , testChecksFail
        ] <*> [tk]


testEmptyConfiguration :: TestTree
testEmptyConfiguration = 
    testCase "empty configuration is correctly parsed to default values" $ do
        let config = defaultConfiguration

        parsedConfig <- configuration "tests/fixtures/.empty-config.yml"
        assertEqual "" config parsedConfig


-- The example configuration is build by hand (to add comments)
-- and it is embedded into the executable. Therefore, we must make sure it 
-- is correctly parsed (and is therefore valid.)
testExampleConfiguration :: TestTree
testExampleConfiguration = 
    testCase "example configuration is correctly parsed" $ do
        -- The example config reflects the Windows default
        -- Therefore, we need to test against the Windows default,
        -- even on other OSes
        let config = defaultConfiguration { matplotlibExe = "python"
                                          , plotlyPythonExe = "python"
                                          , bokehExe = "python"
                                          , defaultDependencies = ["file1.txt", "file2.txt"]
                                          }

        parsedConfig <- configuration "example-config.yml"
        assertEqual "" config parsedConfig


-- Test that the path to configuration in metadata is found correctly
testConfigurationPathMeta :: TestTree
testConfigurationPathMeta = 
    testCase "Configuration path stored in metadata is correctly parsed" $ do
    let configPath = "tests/fixtures/.config-meta.yml"
        meta = B.Meta $ Map.fromList [("plot-configuration", B.MetaString configPath)]

    parsedConfig <- maybe (return defaultConfiguration) configuration $ configurationPathMeta (B.Pandoc meta mempty)
    expected <- configuration (unpack configPath)
    assertEqual "" expected parsedConfig


testCaptionReader :: TestTree
testCaptionReader = 
    testCase "caption is parsed in the same way as input document format" $ do
        -- Note that this test is fragile, in the sense that the expected result must be carefully
        -- constructed
        let caption="Here is a [link](https://www.google.com) in a caption."
            expected = Just $ [Str "Here",Space,Str "is",Space,Str "a",Space,Link ("",[],[]) [Str "link"] ("https://www.google.com",""),Space,Str "in",Space,Str "a",Space,Str "caption."]
            fmt = B.Format "markdown+tex_math_dollars"
            parsed = captionReader fmt caption

        assertEqual "" expected parsed


testHtmlBodyEmbedding :: TestTree
testHtmlBodyEmbedding = 
    testCase "HTML body can be extracted from file" $ do
        let html=T.unlines ["<!DOCTYPE html>"
                           ,"<html lang=\"en\">"
                           ,"  <head>"
                           ,"    <meta charset=\"utf-8\">"
                           ,"    <title>title</title>"
                           ,"    <link rel=\"stylesheet\" href=\"style.css\">"
                           ,"  </head>"
                           ,"  <body>"
                           ,"    <p>Hello</p>"
                           ,"  </body>"
                           ,"</html>"
                           ]
            extracted = extractPlot html
            expected = "\n    <p>Hello</p>"

        assertEqual "" expected extracted 


testHtmlEmbedding :: TestTree
testHtmlEmbedding = 
    testCase "HTML body and head scripts can be extracted from file" $ do
        let html=T.unlines ["<!DOCTYPE html>"
                           ,"<html lang=\"en\">"
                           ,"  <head>"
                           ,"    <meta charset=\"utf-8\">"
                           ,"    <title>title</title>"
                           ,"    <link rel=\"stylesheet\" href=\"style.css\">"
                           ,"    <script src=\"script.js\"></script>"
                           ,"  </head>"
                           ,"  <body>"
                           ,"    <p>Hello</p>"
                           ,"  </body>"
                           ,"</html>"
                           ]
            extracted = extractPlot html
            expected = "<script src=\"script.js\" defer></script>\n    <p>Hello</p>"

        assertEqual "" expected extracted