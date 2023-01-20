{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Monad (unless, when)
import Data.List (isInfixOf, isSuffixOf, (!!))
import Data.Monoid ((<>))
import qualified Data.Set as S
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
  )
import System.FilePath (takeExtensions, (</>))
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Definition as B
import Text.Pandoc.Filter.Plot
import Text.Pandoc.Filter.Plot.Internal
import Text.Pandoc.JSON

defaultTestConfig :: Configuration
defaultTestConfig =
  defaultConfiguration
    { logVerbosity = Silent,
      logSink = StdErr
    }

-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
testFileCreation :: Toolkit -> TestTree
testFileCreation tk =
  testCase "writes output files in appropriate directory" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-file-creation-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let cb = (addDirectory tempDir $ codeBlock tk (trivialContent tk))
    _ <- runPlotM Nothing defaultTestConfig $ make cb
    filesCreated <- length <$> listDirectory tempDir
    assertEqual "" 2 filesCreated

-------------------------------------------------------------------------------
-- Test that plot files and source files are created when the filter is run
-- and the path of the files involves spaces. See Issue #2
testFileCreationPathWithSpaces :: Toolkit -> TestTree
testFileCreationPathWithSpaces tk =
  testCase "writes output files in appropriate directory (with spaces)" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-file-creation-with-spaces-   -" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let cb = (addDirectory tempDir $ codeBlock tk (trivialContent tk))
    _ <- runPlotM Nothing defaultTestConfig $ make cb
    filesCreated <- length <$> listDirectory tempDir
    assertEqual "" 2 filesCreated

-------------------------------------------------------------------------------
-- Test that pandoc-plot appropriately transforms code blocks that are
-- nested in other blocks (e.g. Divs)
testNestedCodeBlocks :: Toolkit -> TestTree
testNestedCodeBlocks tk =
  testCase "transforms code blocks nested in other blocks" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-nester-blocks-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let block =
          Div mempty $
            singleton $
              addDirectory tempDir $
                codeBlock tk (trivialContent tk)
    _ <- runPlotM Nothing defaultTestConfig $ make block
    filesCreated <- length <$> listDirectory tempDir
    assertEqual "" 2 filesCreated
  where
    singleton :: a -> [a]
    singleton = return

-------------------------------------------------------------------------------
-- Test that included files are found within the source
testFileInclusion :: Toolkit -> TestTree
testFileInclusion tk =
  testCase "includes plot inclusions" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-file-inclusion-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let cb =
          ( addPreamble (include tk) $
              addDirectory tempDir $ codeBlock tk (trivialContent tk)
          )
    _ <- runPlotM Nothing defaultTestConfig $ make cb
    inclusion <- readFile (include tk)
    sourcePath <- head . filter (isExtensionOf ".src.html") <$> listDirectory tempDir
    src <- readFile (tempDir </> sourcePath)
    assertIsInfix inclusion src
  where
    include Matplotlib = "tests/includes/matplotlib.py"
    include PlotlyPython = "tests/includes/plotly-python.py"
    include PlotlyR = "tests/includes/plotly-r.r"
    include Matlab = "tests/includes/matlabplot.m"
    include Mathematica = "tests/includes/mathplot.m"
    include Octave = "tests/includes/octave.m"
    include GGPlot2 = "tests/includes/ggplot2.r"
    include GNUPlot = "tests/includes/gnuplot.gp"
    include Graphviz = "tests/includes/graphviz.dot"
    include Bokeh = "tests/includes/bokeh.py"
    include Plotsjl = "tests/includes/plotsjl.jl"
    include PlantUML = "tests/includes/plantuml.txt"
    include SageMath = "tests/includes/sagemath.sage"

-------------------------------------------------------------------------------
-- Test that the files are saved in the appropriate format
testSaveFormat :: Toolkit -> TestTree
testSaveFormat tk =
  testCase "saves in the appropriate format" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-safe-format-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir
    let fmt = head (supportedSaveFormats tk)
        cb =
          ( addSaveFormat fmt $
              addDirectory tempDir $ codeBlock tk (trivialContent tk)
          )
    _ <- runPlotM Nothing defaultTestConfig $ make cb
    numberjpgFiles <-
      length <$> filter (isExtensionOf (extension fmt))
        <$> listDirectory tempDir
    assertEqual "" numberjpgFiles 1

-------------------------------------------------------------------------------
-- Test that the appropriate error is raised when trying to save figures
-- in an incompatible format
testSaveFormatIncompatibility :: Toolkit -> TestTree
testSaveFormatIncompatibility tk = 
  testCase "raises the appropriate error on save format incompatibility" $ do
    let allSaveFormats = enumFromTo minBound maxBound :: [SaveFormat]
        incompatibleFormats = S.toList $ S.difference (S.fromList allSaveFormats) (S.fromList $ supportedSaveFormats tk)
    if null incompatibleFormats
      then return ()
      else do
        let fmt = head incompatibleFormats
            cb = addSaveFormat fmt $ codeBlock tk (trivialContent tk)

        result <- runPlotM Nothing defaultTestConfig $ makeEither cb
        let expectedCheck :: Either PandocPlotError a -> Bool
            expectedCheck (Left (IncompatibleSaveFormatError fmt' tk')) = (fmt' == fmt) && (tk' == tk)
            expectedCheck _ = False
        assertBool "" (expectedCheck result)

-------------------------------------------------------------------------------
-- Test that it is possible to not render source links in captions
testWithSource :: Toolkit -> TestTree
testWithSource tk =
  testCase "appropriately omits links to source code" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-caption-links-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let expected = "caption content"
        noSource =
          addWithSource False $
            addDirectory tempDir $
              addCaption expected $
                codeBlock tk (trivialContent tk)
        withSource =
          addWithSource True $
            addDirectory tempDir $
              addCaption expected $
                codeBlock tk (trivialContent tk)
    blockNoSource   <- runPlotM Nothing defaultTestConfig $ make noSource
    blockWithSource <- runPlotM Nothing defaultTestConfig $ make withSource

    -- In the case where source=false, the caption is used verbatim.
    -- Otherwise, links will be appended to the caption; hence, the caption
    -- is no longer equal to the initial value
    assertEqual    "" [B.Plain $ B.toList (fromString expected)] (extractCaption blockNoSource)
    assertNotEqual "" [B.Plain $ B.toList (fromString expected)] (extractCaption blockWithSource)
  where
    extractCaption (B.Figure _ (Caption _ caption) _) = caption 

-------------------------------------------------------------------------------
-- Test that it is possible to change the source code label in captions
testSourceLabel :: Toolkit -> TestTree
testSourceLabel tk =
  testCase "appropriately changes the source code label" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-source-label-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    -- Note that this test requires that the actual caption be empty
    -- so that the caption is only the source code label
    let withSource =
          addWithSource True $
            addDirectory tempDir $
              addCaption mempty $ -- This test requires that the actual caption be empty
                codeBlock tk (trivialContent tk)
    blockWithSource <- runPlotM Nothing defaultTestConfig {sourceCodeLabel = "Test label"} $ make withSource

    let [Plain [Space, _, B.Link _ ils _, _]] = extractCaption blockWithSource
    assertEqual "" (B.toList $ B.str "Test label") ils
  where
    extractCaption (B.Figure _ (Caption _ caption) _) = caption 

    linkLabel (B.Plain [B.Link _ ils _]) = B.fromList ils
    linkLabel _ = mempty

-------------------------------------------------------------------------------
-- Test that parameters in code blocks will override the defaults in configuration
testOverrideConfiguration :: Toolkit -> TestTree
testOverrideConfiguration tk =
  -- We set the default save format to JPG via the configuration,
  -- but set the code block parameter to PNG.
  -- Therefore, after the filter has been used, there should be one PNG file and
  -- no JPG files.
  testCase "code block attributes override configuration defaults" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-caption-links-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let config =
          defaultTestConfig
            { defaultDirectory = tempDir,
              defaultSaveFormat = JPG
            }

    -- Not all toolkits support both save formats
    when
      ( JPG `elem` supportedSaveFormats tk
          && PNG `elem` supportedSaveFormats tk
      )
      $ do
        let cb =
              addDirectory tempDir $
                addSaveFormat PNG $
                  codeBlock tk (trivialContent tk)
        _ <- runPlotM Nothing config $ make cb

        numberPngFiles <-
          length <$> filter (isExtensionOf (extension PNG))
            <$> listDirectory (defaultDirectory config)
        numberJpgFiles <-
          length <$> filter (isExtensionOf (extension JPG))
            <$> listDirectory (defaultDirectory config)
        assertEqual "" numberPngFiles 1
        assertEqual "" numberJpgFiles 0

-------------------------------------------------------------------------------
-- Test that Markdown bold formatting in captions is correctly rendered
testMarkdownFormattingCaption1 :: Toolkit -> TestTree
testMarkdownFormattingCaption1 tk =
  testCase "appropriately parses captions 1" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-caption-parsing1-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    -- Note that this test is fragile, in the sense that the expected result must be carefully
    -- constructed
    let expected = [B.Plain [B.Strong [B.Str "caption"]]]
        cb =
          addDirectory tempDir $
            addCaption "**caption**" $
              codeBlock tk (trivialContent tk)
        fmt = B.Format "markdown"
    result <- runPlotM Nothing (defaultTestConfig {captionFormat = fmt}) $ make cb
    assertIsInfix expected (extractCaption result)
  where
    extractCaption (B.Figure _ (Caption _ caption) _) = caption 

-------------------------------------------------------------------------------
-- Test that Markdown bold formatting in captions is correctly rendered
testMarkdownFormattingCaption2 :: Toolkit -> TestTree
testMarkdownFormattingCaption2 tk =
  testCase "appropriately parses captions 2" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-caption-parsing2-" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    -- Note that this test is fragile, in the sense that the expected result must be carefully
    -- constructed
    let expected = [B.Plain [Link ("", [], []) [Str "title"] ("https://google.com", "")]]
        cb =
          addDirectory tempDir $
            addCaption "[title](https://google.com)" $
              codeBlock tk (trivialContent tk)
        fmt = B.Format "markdown"
    result <- runPlotM Nothing (defaultTestConfig {captionFormat = fmt}) $ make cb
    assertIsInfix expected (extractCaption result)
  where
    extractCaption (B.Figure _ (Caption _ caption) _) = caption 

-------------------------------------------------------------------------------
-- Test that cleanOutpuDirs correctly cleans the output directory specified in a block.
testCleanOutputDirs :: Toolkit -> TestTree
testCleanOutputDirs tk =
  testCase "correctly cleans output directories" $ do
    let postfix = unpack . cls $ tk
    tempDir <- (</> "test-clean-output-dir" <> postfix) <$> getTemporaryDirectory
    ensureDirectoryExistsAndEmpty tempDir

    let cb =
          addDirectory tempDir $
            codeBlock tk (trivialContent tk)

    result <- runPlotM Nothing defaultTestConfig $ make cb
    cleanedDirs <- cleanOutputDirs defaultTestConfig cb

    assertEqual "" [tempDir] cleanedDirs

    outputDirExists <- doesDirectoryExist tempDir
    assertEqual "" outputDirExists False

-------------------------------------------------------------------------------
-- Test that toolkit checks failed when appropriate.
testChecksFail :: Toolkit -> TestTree
testChecksFail tk =
  testCase "script checks fail when appropriate" $ do
    assertChecksFail tk
  where
    assertChecksFail Matplotlib = do
      let postfix = unpack . cls $ tk
      tempDir <- (</> "test-checks" <> postfix) <$> getTemporaryDirectory
      ensureDirectoryExistsAndEmpty tempDir

      let cb = addDirectory tempDir $ codeBlock Matplotlib "plt.show()"
      result <- runPlotM Nothing defaultTestConfig $ makeEither cb
      let expectedCheck :: Either PandocPlotError a -> Bool
          expectedCheck (Left (ScriptChecksFailedError _)) = True
          expectedCheck _ = False
      assertBool "" (expectedCheck result)
    assertChecksFail _ = assertEqual "Test skipped" True True

codeBlock :: Toolkit -> Script -> Block
codeBlock tk script = CodeBlock (mempty, [cls tk], mempty) script

trivialContent :: Toolkit -> Script
trivialContent Matplotlib = "import matplotlib.pyplot as plt\n"
trivialContent PlotlyPython = "import plotly.graph_objects as go; fit = go.Figure()\n"
trivialContent PlotlyR = "library(plotly)\nfig <- plot_ly(midwest, x = ~percollege, color = ~state, type = \"box\")"
trivialContent Matlab = "figure('visible', 'off')\n"
trivialContent Mathematica = "\n"
trivialContent Octave = "figure('visible', 'off')\nplot (-10:0.1:10);"
trivialContent GGPlot2 = "library(ggplot2)\nggplot()\n"
trivialContent GNUPlot = "plot sin(x)"
trivialContent Graphviz = "digraph {A -> B [label=\"test\"];}"
trivialContent Bokeh =
  T.unlines
    [ "from bokeh.plotting import figure",
      "p = figure(title='simple line example')",
      "p.line([1,2,3,4], [5,6,7,8])"
    ]
trivialContent Plotsjl = "using Plots; x = 1:10; y = rand(10); plot(x, y);"
trivialContent PlantUML = "@startuml\nAlice -> Bob: test\n@enduml"
trivialContent SageMath = "G = plot(sin, 1, 10)"

addCaption :: String -> Block -> Block
addCaption caption (CodeBlock (id', cls, attrs) script) =
  CodeBlock (id', cls, attrs ++ [(tshow CaptionK, pack caption)]) script

addDirectory :: FilePath -> Block -> Block
addDirectory dir (CodeBlock (id', cls, attrs) script) =
  CodeBlock (id', cls, attrs ++ [(tshow DirectoryK, pack dir)]) script

addPreamble :: FilePath -> Block -> Block
addPreamble inclusionPath (CodeBlock (id', cls, attrs) script) =
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
  unless
    (expected /= actual)
    (assertFailure $ mconcat [msg, ": expected ", show expected, " but got ", show actual])

-- | Not available with GHC < 8.4
-- since this function was added in filepath-1.4.2
-- but GHC 8.2.2 comes with filepath-1.4.1.2
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.' : _) = isSuffixOf ext . takeExtensions
isExtensionOf ext = isSuffixOf ('.' : ext) . takeExtensions

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
