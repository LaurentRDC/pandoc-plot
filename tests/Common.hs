{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Control.Monad                    (unless)
import           Control.Monad.Reader

import           Data.Default.Class               (def)
import           Data.List                        (isInfixOf, isSuffixOf)
import           Data.Monoid                      ((<>))
import           Data.String                      (fromString)
import           Data.Text                        (Text, pack, unpack)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.Pandoc.Filter.Plot
import           Text.Pandoc.Filter.Plot.Internal

import qualified Text.Pandoc.Builder              as B
import qualified Text.Pandoc.Definition           as B
import           Text.Pandoc.JSON

import           System.Directory                 (createDirectory,
                                                   createDirectoryIfMissing,
                                                   doesDirectoryExist,
                                                   doesFileExist, listDirectory,
                                                   removeDirectoryRecursive,
                                                   removePathForcibly)
import           System.FilePath                  (takeExtensions, (</>))
import           System.IO.Temp                   (getCanonicalTemporaryDirectory)


-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
testFileCreation :: Toolkit -> TestTree
testFileCreation tk =
    testCase "writes output files in appropriate directory" $ do
        tempDir <- (</> "test-file-creation-" <> show tk) <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let cb = (addDirectory tempDir $ codeBlock tk (trivialContent tk))
        _ <- (make tk) def cb
        filesCreated <- length <$> listDirectory tempDir
        assertEqual "" 2 filesCreated

-------------------------------------------------------------------------------
-- Test that included files are found within the source
testFileInclusion :: Toolkit -> TestTree
testFileInclusion tk =
    testCase "includes plot inclusions" $ do
        tempDir <- (</> "test-file-inclusion-" <> show tk) <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let cb = (addInclusion (include tk) $
                    addDirectory tempDir $ codeBlock tk (trivialContent tk))
        _ <- (make tk) def cb
        inclusion <- readFile (include tk)
        sourcePath <- head . filter (isExtensionOf "txt") <$> listDirectory tempDir
        src <- readFile (tempDir </> sourcePath)
        assertIsInfix inclusion src
    where
        include Matplotlib   = "tests/includes/matplotlib.py"
        include PlotlyPython = "tests/includes/plotly-python.py"
        include Matlab       = "tests/includes/matlabplot.m"
        include Mathematica  = "tests/includes/mathplot.m"
        include Octave       = "tests/includes/octave.m"
        include GGPlot2      = "tests/includes/ggplot2.r"
        include GNUPlot      = "tests/includes/gnuplot.gp"

-------------------------------------------------------------------------------
-- Test that the files are saved in the appropriate format
testSaveFormat :: Toolkit -> TestTree
testSaveFormat tk =
    testCase "saves in the appropriate format" $ do
        tempDir <- (</> "test-safe-format-" <> show tk) <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir
        let fmt = head (supportedSaveFormats tk)
            cb = (addSaveFormat fmt $
                 addDirectory tempDir $ codeBlock tk (trivialContent tk))
        _ <- (make tk) def cb
        numberjpgFiles <-
            length <$> filter (isExtensionOf (extension fmt)) <$>
            listDirectory tempDir
        assertEqual "" numberjpgFiles 1

-------------------------------------------------------------------------------
-- Test that it is possible to not render source links in captions
testWithSource :: Toolkit -> TestTree
testWithSource tk =
    testCase "appropriately omits links to source code" $ do
        tempDir <- (</> "test-caption-links-" <> show tk) <$> getCanonicalTemporaryDirectory
        ensureDirectoryExistsAndEmpty tempDir

        let expected = "caption content"
            noSource = addWithSource False 
                      $ addDirectory tempDir 
                      $ addCaption expected 
                      $ codeBlock tk (trivialContent tk)
            withSource = addWithSource True 
                      $ addDirectory tempDir 
                      $ addCaption expected 
                      $ codeBlock tk (trivialContent tk)
        blockNoSource   <- (make tk) def noSource
        blockWithSource <- (make tk) def withSource

        -- In the case where source=false, the caption is used verbatim.
        -- This is what this test is checking.
        assertEqual    "" (B.toList $ fromString expected) (extractCaption blockNoSource)
        assertNotEqual "" (B.toList $ fromString expected) (extractCaption blockWithSource)

    where
        extractCaption (B.Para blocks) = extractImageCaption . head $ blocks
        extractCaption _               = mempty

        extractImageCaption (Image _ c _) = c
        extractImageCaption _             = mempty


codeBlock :: Toolkit -> Script -> Block
codeBlock tk script = CodeBlock (mempty, [cls tk], mempty) script


trivialContent :: Toolkit -> Script
trivialContent Matplotlib   = "import matplotlib.pyplot as plt\n"
trivialContent PlotlyPython = "import plotly.graph_objects as go; fit = go.Figure()\n"
trivialContent Matlab       = "figure('visible', 'off')\n"
trivialContent Mathematica  = "\n"
trivialContent Octave       = "figure('visible', 'off')\nplot (-10:0.1:10);"
trivialContent GGPlot2      = "library(ggplot2)\nggplot()\n"
trivialContent GNUPlot      = "plot sin(x)"


addCaption :: String -> Block -> Block
addCaption caption (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(tshow CaptionK, pack caption)]) script


addDirectory :: FilePath -> Block -> Block
addDirectory dir (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(tshow DirectoryK, pack dir)]) script


addInclusion :: FilePath -> Block -> Block
addInclusion inclusionPath (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(tshow PreambleK, pack inclusionPath)]) script


addSaveFormat :: SaveFormat -> Block -> Block
addSaveFormat saveFormat (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(tshow SaveFormatK, pack . extension $ saveFormat)]) script


addDPI :: Int -> Block -> Block
addDPI dpi (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(tshow DpiK, pack . show $ dpi)]) script


addWithSource :: Bool -> Block -> Block
addWithSource yn (CodeBlock (id', cls, attrs) script) =
    CodeBlock (id', cls, attrs ++ [(tshow WithSourceK, pack . show $ yn)]) script


-- | Assert that a file exists
assertFileExists :: HasCallStack => FilePath -> Assertion
assertFileExists filepath = do
    fileExists <- doesFileExist filepath
    unless fileExists (assertFailure msg)
  where
    msg = mconcat ["File ", filepath, " does not exist."]


-- | Assert not equal
assertNotEqual :: (HasCallStack, Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual msg expected actual = 
    unless (expected /= actual) 
        (assertFailure $ mconcat [msg, ": expected ", show expected, " but got ", show actual])


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

tshow :: Show a => a -> Text
tshow = pack . show
