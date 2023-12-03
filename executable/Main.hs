{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (join, msum, void, when)
import Data.List (intersperse, (\\))
import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import Data.Version (parseVersion, showVersion)
import qualified Data.Version as V
import Development.GitRev (gitHash)
import ExampleConfig (embedExampleConfig)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import ManPage (embedManualHtml)
import OpenFile (openFile)
import Options.Applicative
  ( Alternative ((<|>)),
    Parser,
    command,
    execParser,
    flag,
    footerDoc,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    short,
    strArgument,
    strOption,
    subparser,
    value,
    (<**>),
  )
import qualified Options.Applicative.Help.Pretty as P
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Pandoc (pandocVersion)
import Text.Pandoc.Definition (pandocTypesVersion)
import Text.Pandoc.Filter.Plot
  ( Configuration (..),
    availableToolkits,
    availableBlockKeys,
    configuration,
    defaultConfiguration,
    pandocPlotVersion,
    plotFilter,
  )
import Text.Pandoc.Filter.Plot.Internal
  ( cleanOutputDirs,
    cls,
    configurationPathMeta,
    executable,
    pathToExe,
    readDoc,
    runPlotM,
    supportedSaveFormats,
    toolkits,
  )
import Text.Pandoc.JSON (toJSONFilter)
import Text.ParserCombinators.ReadP (readP_to_S)

-- The difference between commands and flags is that commands require knowledge of
-- the configuration, while flags only display static information.

-- Please note that for some reason, makeVersion [2, 11, 0, 0] > makeVersion [2, 11]
minimumPandocVersion :: V.Version
minimumPandocVersion = V.makeVersion [2, 11]

data Command
  = Clean (Maybe FilePath) FilePath
  | WriteConfig FilePath
  | Toolkits (Maybe FilePath)
  | BlockKeys

data Flag
  = Version
  | FullVersion
  | Manual
  deriving (Eq)

main :: IO ()
main = do
  setLocaleEncoding utf8
  join $ execParser opts
  where
    opts =
      info
        (optparse <**> helper)
        ( fullDesc
            <> progDesc
              ( unlines
                  [ "This pandoc filter generates plots from code blocks using a multitude of ",
                    "possible renderers. This allows to keep documentation and figures in",
                    "perfect synchronicity."
                  ]
              )
            <> header (mconcat ["pandoc-plot ", V.showVersion pandocPlotVersion, " - generate figures directly in documents"])
            <> footerDoc
              ( Just $
                  P.vsep
                    [ "More information can be found via the manual (pandoc-plot --manual) or the",
                      "repository README, located at https://github.com/LaurentRDC/pandoc-plot"
                    ]
              )
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
    go (Just Version) _ _ = putStrLn (V.showVersion pandocPlotVersion)
    go (Just FullVersion) _ _ = showFullVersion
    go (Just Manual) _ _ = showManPage
    go _ (Just (Toolkits    mfp   )) _ = showAvailableToolkits mfp
    go _ (Just (Clean       mfp fp)) _ = clean mfp fp
    go _ (Just (WriteConfig     fp)) _ = writeFile fp $(embedExampleConfig)
    go _ (Just  BlockKeys          ) _ = showAvailableBlockKeys
    go Nothing Nothing _ = toJSONFilterWithConfig

flagParser :: Parser (Maybe Flag)
flagParser = versionP <|> fullVersionP <|> manualP
  where
    versionP =
      flag
        Nothing
        (Just Version)
        ( mconcat
            [ long "version",
              short 'v',
              help "Show version number and exit."
            ]
        )

    fullVersionP =
      flag
        Nothing
        (Just FullVersion)
        ( mconcat
            [ long "full-version",
              help "Show full version information and exit."
            ]
        )

    manualP =
      flag
        Nothing
        (Just Manual)
        ( mconcat
            [ long "manual",
              short 'm',
              help "Open the manual page in the default web browser and exit."
            ]
        )

commandParser :: Parser (Maybe Command)
commandParser =
  optional $
    subparser $
      mconcat
        [ command
            "toolkits"
            ( info (toolkitsP <**> helper) (progDesc "Show information on toolkits and exit.")
            ),
          command
            "clean"
            ( info
                (cleanP <**> helper)
                ( progDesc
                    ( unlines
                        [ "Clean output directories where figures from FILE and log files might be stored.",
                          "WARNING: All files in those directories will be deleted."
                        ]
                    )
                )
            ),
          command
            "write-example-config"
            ( info (writeConfigP <**> helper) (progDesc "Write example configuration to a file and exit.")
            ),
          command
            "block-keys"
            ( info (blockKeysP <**> helper) (progDesc "Write all supported per-block keys, not just those documented.")
            )
        ]
  where
    configP      = optional $ strOption (mconcat [long "config", metavar "PATH", help "Path to optional configuration file."])
    toolkitsP    = Toolkits <$> configP
    blockKeysP   = pure BlockKeys
    cleanP       = Clean <$> configP <*> strArgument (metavar "FILE")
    writeConfigP =
      WriteConfig
        <$> strOption
          ( mconcat
              [ long "path",
                metavar "FILE",
                value ".example-pandoc-plot.yml",
                help "Target location of the configuration file. Default is \".example-pandoc-plot.yml\""
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
toJSONFilterWithConfig :: IO ()
toJSONFilterWithConfig = do
  upToDatePandoc <- checkRuntimePandocVersion
  when upToDatePandoc $
    toJSONFilter $ \mfmt doc -> do
      c <- maybe localConfig configuration (configurationPathMeta doc)
      plotFilter c mfmt doc

-- | Check that the runtime version of Pandoc is at least 2.11. The return value
-- indicates whether the Pandoc version is new enough or not.
checkRuntimePandocVersion :: IO Bool
checkRuntimePandocVersion = do
  -- Pandoc runs filters in an environment with two variables:
  -- PANDOV_VERSION and PANDOC_READER_OPTS
  -- We can use the former to ensure that people are not
  -- using an old version of pandoc
  pandocV <- lookupEnv "PANDOC_VERSION"
  case pandocV >>= readVersion of
    Nothing -> return True
    Just v ->
      if v < minimumPandocVersion
        then do
          hPutStrLn stderr $
            mconcat
              [ "ERROR (pandoc-plot) The pandoc-plot filter only ",
                "supports Pandoc 2.11 and newer. ",
                "but you are using Pandoc ",
                showVersion v
              ]
          return False
        else return True
  where
    readVersion = fmap fst . lastMaybe . readP_to_S parseVersion
    lastMaybe xs = if length xs > 1 then Just (last xs) else Nothing

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
  putStrLn $ "pandoc-plot " <> V.showVersion pandocPlotVersion
  putStrLn $ "Git revision " <> $gitHash
  putStrLn $
    mconcat
      [ "Compiled with pandoc ",
        V.showVersion pandocVersion,
        " and pandoc-types ",
        V.showVersion pandocTypesVersion,
        " using GHC ",
        TOOL_VERSION_ghc -- Constant defined by CPP
      ]

showAvailableToolkits :: Maybe FilePath -> IO ()
showAvailableToolkits mfp = do
  c <- maybe localConfig configuration mfp

  putStrLn "\nAVAILABLE TOOLKITS\n"
  available <- availableToolkits c
  mapM_ (availToolkitInfo c) available
  putStrLn "\nUNAVAILABLE TOOLKITS\n"
  -- We don't use unavailableToolkits because this would force
  -- more IO actions
  let unavailable = toolkits \\ available
  mapM_ (unavailToolkitInfo c) unavailable
  where
    toolkitInfo avail conf tk = do
      putStrLn $ "Toolkit: " <> show tk
      when avail $ do
        exe <- runPlotM Nothing conf $ executable tk
        putStrLn $ "    Executable: " <> pathToExe exe
      putStrLn $ "    Code block trigger: " <> (unpack . cls $ tk)
      putStrLn $ "    Supported save formats: " <> (mconcat . intersperse ", " . fmap show $ supportedSaveFormats tk)
      putStrLn mempty
    availToolkitInfo = toolkitInfo True
    unavailToolkitInfo = toolkitInfo False

showAvailableBlockKeys :: IO ()
showAvailableBlockKeys = do
  putStrLn "\nAVAILABLE KEYS FOR FIGURE BLOCKS\n"
  mapM_ putStrLn availableBlockKeys

-- | Clean output directories associated with a file
--
-- Priority for configuration are the same as @toJSONFilterWithConfig@.
clean ::
  Maybe FilePath -> -- Use configuration file?
  FilePath -> -- Document to clean
  IO ()
clean mfp fp = do
  doc <- readDoc fp
  -- Note the priority for configuration:
  --   (1) path of argument --config
  --   (2) document metadata
  --   (3) local .pandoc-plot.yml
  --   (4) default config
  conf <- maybe localConfig configuration $ firstJusts [configurationPathMeta doc, mfp]
  void (cleanOutputDirs conf doc)
  where
    firstJusts :: [Maybe a] -> Maybe a
    firstJusts = msum

showManPage :: IO ()
showManPage = do
  manualPath <- (</> "pandoc-plot-manual.html") <$> getTemporaryDirectory
  TIO.writeFile manualPath $(embedManualHtml)
  openFile ("file:///" <> manualPath)
