{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (join, forM_)

import           Data.List                        (intersperse, (\\))
import           Data.Monoid                      ((<>))
import           Data.Text                        (unpack)

import           GitHash                          as Git

import           Options.Applicative
import qualified Options.Applicative.Help.Pretty  as P

import           System.Directory                 (doesFileExist)
import           System.IO.Temp                   (writeSystemTempFile)

import           Text.Pandoc.Filter.Plot          (availableToolkits,
                                                   plotTransform,
                                                   defaultConfiguration, 
                                                   configuration, 
                                                   Configuration(..))
import           Text.Pandoc.Filter.Plot.Internal (cls, supportedSaveFormats, 
                                                   toolkits, readDoc, 
                                                   cleanOutputDirs, 
                                                   configurationPathMeta)

import           Text.Pandoc                      (pandocVersion)
import           Text.Pandoc.Definition           (pandocTypesVersion)
import           Text.Pandoc.JSON                 (toJSONFilter)

import           Web.Browser                      (openBrowser)

import qualified Data.Version                     as V
import           Paths_pandoc_plot                (version)

import           ManPage                          (embedManualHtml)
import           ExampleConfig                    (embedExampleConfig)

-- The difference between commands and flags is that commands require knowledge of
-- the configuration, while flags only display static information.

data Command = Clean FilePath
             | WriteConfig FilePath
             | Toolkits

data Flag = Version
          | FullVersion
          | Manual
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
        go (Just Version)          _ _ = putStrLn (V.showVersion version)
        go (Just FullVersion)      _ _ = showFullVersion
        go (Just Manual)           _ _ = showManPage
        go _ (Just Toolkits)         _ = showAvailableToolkits
        go _ (Just (Clean fp))       _ = clean fp
        go _ (Just (WriteConfig fp)) _ = writeFile fp $(embedExampleConfig)
        go Nothing Nothing           _ = toJSONFilterWithConfig

flagParser :: Parser (Maybe Flag)
flagParser = versionP <|> fullVersionP <|> manualP
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

commandParser :: Parser (Maybe Command)
commandParser = optional $ subparser $ mconcat
            [ command "toolkits" ( 
                info (toolkitsP <**> helper) (progDesc "Show information on toolkits and exit.")
                )  
            , command "clean" (
                info (cleanP <**> helper) ( 
                    progDesc "Clean output directories where figures from FILE might be stored.\
                              \ WARNING: All files in those directories will be deleted." 
                    )
                )
            , command "write-example-config" (
                info (writeConfigP <**> helper) (progDesc "Write example configuration to a file and exit.")
                )
            ]
    where
        toolkitsP = pure Toolkits
        cleanP = Clean <$> strArgument (metavar "FILE")
        writeConfigP = WriteConfig <$> 
                strOption ( 
                    mconcat [ long "path"
                            , metavar "FILE"
                            , value ".example-pandoc-plot.yml"
                            , help "Target location of the configuration file. Default is \".example-pandoc-plot.yml\""
                            ] 
                          )

-- | Determine configuration and run filter.
--
-- Priority for configuration:
-- 
--     (1) Loaded from filepath stored in document metadata, under the key @plot-configuration@;
--
--     (2) Loaded from file @.pandoc-plot.yml@ in current work directory;
--
--     (3) Default configuration
--
toJSONFilterWithConfig :: IO ()
toJSONFilterWithConfig = toJSONFilter $ \doc -> do
    c <- maybe localConfig configuration (configurationPathMeta doc)
    plotTransform c doc


-- | Load configuration from local file @.pandoc-plot.yml@. 
-- If the file does not exist, the default configuration will be used.
localConfig :: IO Configuration
localConfig = do 
    configExists <- doesFileExist ".pandoc-plot.yml"
    if configExists
        then configuration ".pandoc-plot.yml"
        else return defaultConfiguration


showFullVersion :: IO ()
showFullVersion = do
    putStrLn $ "pandoc-plot " <> (V.showVersion version)
    putStrLn $ "Git revision " <> gitrev
    putStrLn $ mconcat [ "Compiled with pandoc "
                        , (unpack pandocVersion)
                        , " and pandoc-types "
                        , V.showVersion pandocTypesVersion
                        ]
    where
        -- In certain environments (e.g. Hackage when building documentation),
        -- there is no git information. 
        gitrev = either (const "unknown") Git.giHash ($$tGitInfoCwdTry)


showAvailableToolkits :: IO ()
showAvailableToolkits = do
    c <- localConfig
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
            putStrLn $ "    Code block trigger: " <> (unpack . cls $ tk)
            putStrLn $ "    Supported save formats: " <> (mconcat . intersperse ", " . fmap show $ supportedSaveFormats tk)
            putStrLn mempty


-- | Clean output directories associated with a file
-- 
-- Priority for configuration are the same as @toJSONFilterWithConfig@.
clean :: FilePath -> IO ()
clean fp = do
    doc <- readDoc fp
    conf <- maybe localConfig configuration (configurationPathMeta doc)
    putStrLn $ "Cleaning output directories for " <> fp
    cleanedDirs <- cleanOutputDirs conf doc
    forM_ cleanedDirs $ \d -> putStrLn $ "Removed directory " <> d


showManPage :: IO ()
showManPage = 
    writeSystemTempFile "pandoc-plot-manual.html" $(embedManualHtml)
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