{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

import           Control.Monad (filterM, forM_)

import           Data.List  ((\\))
import           Data.Maybe (isJust)
import           Data.Text (unpack, Text)

import qualified Turtle         as Sh
import qualified Turtle.Prelude as Sh

import           Test.Tasty
import           Test.Tasty.HUnit

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


availableToolkits :: IO [Toolkit]
availableToolkits = filterM toolkitAvailable toolkits
    where
        toolkitAvailable :: Toolkit -> IO Bool
        toolkitAvailable tk = 
            Sh.which (toolkitExecutable tk) >>= (fmap isJust . return)

        -- The @which@ function from Turtle only works on
        -- windows is the executable extension is included.
        whichExt :: Text
#if defined(mingw32_HOST_OS)
        whichExt = ".exe"
#else
        whichExt = mempty
#endif

        toolkitExecutable :: Toolkit -> Sh.FilePath
        toolkitExecutable Matplotlib    = Sh.fromText $ "python" <> whichExt
        toolkitExecutable PlotlyPython  = Sh.fromText $ "python" <> whichExt
        toolkitExecutable Matlab        = Sh.fromText $ "matlab" <> whichExt