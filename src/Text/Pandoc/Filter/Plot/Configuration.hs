{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : MIT
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Configuration for pandoc-plot
-}

module Text.Pandoc.Filter.Plot.Configuration (
      Configuration(..)
    , directoryKey
    , captionKey
    , dpiKey
    , includePathKey
    , saveFormatKey
    , withLinksKey
) where

import Text.Pandoc.Filter.Plot.Types

import           Data.Default.Class              (Default, def)
import qualified Data.Text                       as T
import           Data.Yaml
import           Data.Yaml.Config                (ignoreEnv, loadYamlSettings)


-- | Keys that pandoc-plot will look for in code blocks. These are only exported for testing purposes.
directoryKey, captionKey, dpiKey, includePathKey, saveFormatKey, withLinksKey, isTightBboxKey, isTransparentKey :: String
directoryKey     = "directory"
captionKey       = "caption"
dpiKey           = "dpi"
includePathKey   = "include"
saveFormatKey    = "format"
withLinksKey     = "links"
isTightBboxKey   = "tight_bbox"
isTransparentKey = "transparent"


-- | list of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [String]
inclusionKeys = [ directoryKey
                , captionKey
                , dpiKey
                , includePathKey
                , saveFormatKey
                , withLinksKey
                ]


data Configuration = Configuration
    { defaultDirectory    :: FilePath
    , defaultIncludePath  :: Maybe FilePath
    , defaultWithLinks    :: Bool
    , defaultSaveFormat   :: SaveFormat
    , pythonInterpreter   :: String
    , matplotlibConfig    :: MatplotlibConfig
    }


data MatplotlibConfig = MatplotlibConfig
    { defaultDPI    :: Int  -- ^ The default dots-per-inch value for generated figures. Matplotlib only, ignored otherwise.
    , isTightBbox   :: Bool -- ^ Whether the figures should be saved with @bbox_inches="tight"@ or not. Useful for larger figures with subplots. Matplotlib only, ignored otherwise.
    , isTransparent :: Bool -- ^ If True, figures will be saved with transparent background rather than solid color. .Matplotlib only, ignored otherwise.
    }
    deriving (Eq, Show)


instance Default Configuration where
    def = Configuration
        { defaultDirectory   = "generated/"
        , defaultIncludePath = Nothing
        , defaultWithLinks   = True
        , defaultSaveFormat  = PNG
        , pythonInterpreter  = defaultPythonInterpreter
        , matplotlibConfig   = def
    }

instance Default MatplotlibConfig where
    def = MatplotlibConfig {
          defaultDPI           = 80
        , isTightBbox          = False
        , isTransparent        = False
        }

-- | A @Configuration@ cannot be directly created from a YAML file
-- for two reasons:
--
--     * we want to store an include script. However, it makes more sense to
--       specify the script path in a YAML file.
--     * Save format is best specified by a string, and this must be parsed later
--
-- Therefore, we have another type, ConfigPrecursor, which CAN be created directly from
-- a YAML file.
data ConfigPrecursor
    = ConfigPrecursor
        { defaultDirectory_   :: FilePath
        , defaultIncludePath_ :: Maybe FilePath
        , defaultWithLinks_   :: Bool
        , defaultSaveFormat_  :: String
        , pythonInterpreter_  :: String
        , matplotlibConfig_   :: MatplotlibConfig
        }

instance FromJSON ConfigPrecursor where
    parseJSON (Object v) =
        ConfigPrecursor
            <$> v .:? (T.pack directoryKey)     .!= (defaultDirectory def)
            <*> v .:? (T.pack includePathKey)
            <*> v .:? (T.pack withLinksKey)     .!= (defaultWithLinks def)
            <*> v .:? (T.pack saveFormatKey)    .!= (extension $ defaultSaveFormat def)
            <*> v .:? "interpreter"             .!= defaultPythonInterpreter
            <*> v .:? "matplotlib"              .!= def

    parseJSON _ = fail "Could not parse the configuration"

instance FromJSON MatplotlibConfig where
    parseJSON (Object v) =
        MatplotlibConfig
            <$> v .:? (T.pack dpiKey)           .!= (defaultDPI def)
            <*> v .:? (T.pack isTightBboxKey)   .!= (isTightBbox def)
            <*> v .:? (T.pack isTransparentKey) .!= (isTransparent def)

    parseJSON _ = fail "Could not parse the configuration"

-- | Default interpreter should be Python 3, which has a different
-- name on Windows ("python") vs Unix ("python3")
defaultPythonInterpreter :: String
#if defined(mingw32_HOST_OS)
defaultPythonInterpreter = "python"
#else
defaultPythonInterpreter = "python3"
#endif