{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (join, forM_)

import           Data.Default.Class               (def)
import           Data.List                        (intersperse, (\\))
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T

import           GitHash                          as Git

import           Options.Applicative
import qualified Options.Applicative.Help.Pretty  as P

import           System.Directory                 (doesFileExist)
import           System.IO.Temp                   (writeSystemTempFile)

import           Text.Pandoc.Filter.Plot          (availableToolkits,
                                                   plotTransform)
import           Text.Pandoc.Filter.Plot.Internal (cls, Configuration(..),
                                                   supportedSaveFormats, 
                                                   configuration, toolkits, 
                                                   readDoc, cleanOutputDirs)

import           Text.Pandoc                      (pandocVersion)
import           Text.Pandoc.Definition           (pandocTypesVersion)
import           Text.Pandoc.JSON                 (toJSONFilter)

import           Web.Browser                      (openBrowser)

import qualified Data.Version                     as V
import           Paths_pandoc_plot                (version)

import           ManPage                          (embedManualHtml)
import           ExampleConfig                    (embedExampleConfig)

-- It is understood that Opts Nothing Nothing should be used for filtering
data Opts = Opts
    { optCommand :: Maybe Command
    , optFlag :: Maybe Flag
    }

-- The difference between commands and flags is that commands perform actions,
-- while flags only display information.

data Command = Clean FilePath
             | WriteConfig

data Flag = Version
          | FullVersion
          | Manual
          | Toolkits
    deriving (Eq)


main :: IO ()
main = join $ execParser opts
    where 
        opts = info (optparse <**> helper)
            (fullDesc
            <> progDesc "This pandoc filter generates plots from code blocks using a multitude of possible renderers. \
                        \This allows to keep documentation and figures in perfect synchronicity."
            <> header "pandoc-plot - generate figures directly in documents using your plotting toolkit of choice."
            <> footerDoc (Just footer')
            )
        
        optparse = do
            flag_ <- flagParser
            command_ <- commandParser
            -- The extra optional input below only serves to show
            -- to the user that the last argument is the AST from pandoc
            -- The parsed input is never used
            input <- optional $ strArgument (metavar "AST")
            return $ go flag_ command_ input
        
        go :: Maybe Flag -> Maybe Command -> Maybe String -> IO ()
        go (Just Version)     _ _ = putStrLn (V.showVersion version)
        go (Just FullVersion) _ _ = showFullVersion
        go (Just Manual)      _ _ = showManPage
        go (Just Toolkits)    _ _ = showAvailableToolkits
        go _ (Just (Clean fp))  _ = clean fp
        go _ (Just WriteConfig) _ = T.writeFile ".example-pandoc-plot.yml" exampleConfig
        go Nothing Nothing      _ = toJSONFilterWithConfig

flagParser :: Parser (Maybe Flag)
flagParser = versionP <|> fullVersionP <|> manualP <|> toolkitsP
    where
        versionP = flag Nothing (Just Version) (mconcat
            [ long "version"
            , short 'v'
            , help "Show version number and exit."
            ])
        
        fullVersionP = flag Nothing (Just FullVersion) (mconcat
            [ long "full-version"
            , help "Show full version information and exit."
            ])

        manualP  = flag Nothing (Just Manual) (mconcat
            [ long "manual"
            , short 'm'
            , help "Open the manual page in the default web browser and exit."
            ])

        toolkitsP = flag Nothing (Just Toolkits) (mconcat
            [ long "toolkits"
            , short 't'
            , help "Show information on toolkits and exit. Executables from the configuration \
                   \file will be used, if a '.pandoc-plot.yml' file is in the current directory."
            ])

commandParser :: Parser (Maybe Command)
commandParser = optional $ subparser (
            command "clean" (
                info (cleanP <**> helper) ( 
                    progDesc "Clean output directories where figures from FILE might be stored.\
                              \ WARNING: All files in those directories will be deleted." 
                    )
                )
            <> command "write-example-config" (
                    info (writeConfigP <**> helper) (progDesc "Write example configuration to a file.")
                    )
                )
    where
        cleanP = Clean <$> strArgument (metavar "FILE")
        writeConfigP = pure WriteConfig


toJSONFilterWithConfig :: IO ()
toJSONFilterWithConfig = do
    c <- config
    toJSONFilter (plotTransform c)


config :: IO Configuration
config = do 
    configExists <- doesFileExist ".pandoc-plot.yml"
    if configExists
        then configuration ".pandoc-plot.yml" 
        else return def


manualHtml :: T.Text
manualHtml = T.pack $(embedManualHtml)


exampleConfig :: T.Text
exampleConfig = T.pack $(embedExampleConfig)


showFullVersion :: IO ()
showFullVersion = do
    putStrLn $ "pandoc-plot " <> (V.showVersion version)
    putStrLn $ "Git revision " <> gitrev
    putStrLn $ mconcat [ "Compiled with pandoc "
                        , (T.unpack pandocVersion)
                        , " and pandoc-types "
                        , V.showVersion pandocTypesVersion
                        ]
    where
        -- In certain environments (e.g. Hackage when building documentation),
        -- there is no git information. 
        gitrev = either (const "unknown") Git.giHash ($$tGitInfoCwdTry)


showAvailableToolkits :: IO ()
showAvailableToolkits = do
    c <- config
    putStrLn "\nAVAILABLE TOOLKITS\n"
    available <- availableToolkits c
    return available >>= mapM_ toolkitInfo
    putStrLn "\nUNAVAILABLE TOOLKITS\n"
    -- We don't use unavailableToolkits because this would force
    -- more IO actions
    let unavailable = toolkits \\ available
    return unavailable >>= mapM_ toolkitInfo
    where
        toolkitInfo tk = do
            putStrLn $ "Toolkit: " <> show tk
            putStrLn $ "    Code block trigger: " <> (T.unpack . cls $ tk)
            putStrLn $ "    Supported save formats: " <> (mconcat . intersperse ", " . fmap show $ supportedSaveFormats tk)
            putStrLn mempty


clean :: FilePath -> IO ()
clean fp = do
    conf <- config
    putStrLn $ "Cleaning output directories for " <> fp
    cleanedDirs <- readDoc fp >>= cleanOutputDirs conf
    forM_ cleanedDirs $ \d -> putStrLn $ "Removed directory " <> d


showManPage :: IO ()
showManPage = 
    writeSystemTempFile "pandoc-plot-manual.html" (T.unpack manualHtml)
        >>= \fp -> openBrowser ("file:///" <> fp)
        >> return ()

-- | Use Doc type directly because of newline formatting
footer' :: P.Doc
footer' = mconcat 
    [ P.text "More information can be found via the manual (pandoc-plot --manual) or the repository README, located at"
    , P.line
    , P.indent 4 $ P.text "https://github.com/LaurentRDC/pandoc-plot"
    , P.line
    ]