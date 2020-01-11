{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad                      (unless)
import           Control.Monad.Reader

import           Data.Default.Class                 (def)
import           Data.List                          (isInfixOf, isSuffixOf)
import           Data.Monoid                        ((<>))
import           Data.Text                          (unpack, pack)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.Pandoc.Filter.Plot
import           Text.Pandoc.Filter.Plot.Internal

import qualified Text.Pandoc.Builder                as B
import qualified Text.Pandoc.Definition             as B
import           Text.Pandoc.JSON

import           System.Directory                   (createDirectory,
                                                     createDirectoryIfMissing,
                                                     doesDirectoryExist,
                                                     doesFileExist,
                                                     listDirectory,
                                                     removeDirectoryRecursive,
                                                     removePathForcibly)
import           System.FilePath                    (takeExtensions, (</>))
import           System.IO.Temp                     (getCanonicalTemporaryDirectory)

main :: IO ()
main =
    defaultMain $
    testGroup
        "Text.Pandoc.Filter.Plot"
        [ testFileCreation
        ]


-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
testFileCreation :: TestTree
testFileCreation =
    testCase "writes output files in appropriate directory" $ do
        tempDir <- (</> "test-file-creation") <$> return "" --getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let codeBlock = (addDirectory tempDir $ plotCodeBlock "import matplotlib.pyplot as plt\n")
        _ <- makeMatplotlib Nothing codeBlock
        filesCreated <- length <$> listDirectory tempDir
        assertEqual "" 3 filesCreated



plotCodeBlock :: Script -> Block
plotCodeBlock script = CodeBlock (mempty, ["matplotlib"], mempty) script


addCaption :: String -> Block -> Block
addCaption caption (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(captionKey, pack caption)]) script


addDirectory :: FilePath -> Block -> Block
addDirectory dir (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(directoryKey, pack dir)]) script


addInclusion :: FilePath -> Block -> Block
addInclusion inclusionPath (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(preambleKey, pack inclusionPath)]) script


addSaveFormat :: SaveFormat -> Block -> Block
addSaveFormat saveFormat (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(saveFormatKey, pack . extension $ saveFormat)]) script


addDPI :: Int -> Block -> Block
addDPI dpi (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(dpiKey, pack . show $ dpi)]) script


addWithLinks :: Bool -> Block -> Block
addWithLinks yn (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(withLinksKey, pack . show $ yn)]) script


-- | Assert that a file exists
assertFileExists :: HasCallStack => FilePath -> Assertion
assertFileExists filepath = do
    fileExists <- doesFileExist filepath
    unless fileExists (assertFailure msg)
  where
    msg = mconcat ["File ", filepath, " does not exist."]


-- | Not available with GHC < 8.4
-- since this function was added in filepath-1.4.2
-- but GHC 8.2.2 comes with filepath-1.4.1.2
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions


-- | Assert that the first list is contained,
-- wholly and intact, anywhere within the second.
assertIsInfix :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> Assertion
assertIsInfix xs ys = unless (xs `isInfixOf` ys) (assertFailure msg)
  where
    msg = mconcat ["Expected ", show xs, " to be an infix of ", show ys]

-- Ensure a directory is empty but exists.
ensureDirectoryExistsAndEmpty :: FilePath -> IO ()
ensureDirectoryExistsAndEmpty dir = do
    exists <- doesDirectoryExist dir
    if exists
        then removePathForcibly dir
        else return ()
    createDirectory dir