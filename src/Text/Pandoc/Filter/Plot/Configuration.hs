{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Configuration for pandoc-plot
-}

module Text.Pandoc.Filter.Plot.Configuration (
      Configuration(..)
    , directoryKey
    , captionKey
    , includePathKey
    , saveFormatKey
    , withLinksKey
) where

import Text.Pandoc.Filter.Plot.Types
import Text.Pandoc.Filter.Plot.Renderers

import           Data.Default.Class              (Default, def)
import           Data.String                     (IsString)
import qualified Data.Text                       as T
import           Data.Yaml
import           Data.Yaml.Config                (ignoreEnv, loadYamlSettings)


-- | Keys that pandoc-plot will look for in code blocks. These are only exported for testing purposes.
directoryKey, captionKey, includePathKey, saveFormatKey, withLinksKey :: IsString a => a
directoryKey     = "directory"
captionKey       = "caption"
includePathKey   = "include"
saveFormatKey    = "format"
withLinksKey     = "links"


-- | list of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [String]
inclusionKeys = [ directoryKey
                , captionKey
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


instance Default Configuration where
    def = Configuration
        { defaultDirectory   = "generated/"
        , defaultIncludePath = Nothing
        , defaultWithLinks   = True
        , defaultSaveFormat  = PNG
        , pythonInterpreter  = defaultPythonInterpreter
        , matplotlibConfig   = def
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
            <$> v .:? directoryKey   .!= (defaultDirectory def)
            <*> v .:? includePathKey
            <*> v .:? withLinksKey   .!= (defaultWithLinks def)
            <*> v .:? saveFormatKey  .!= (extension $ defaultSaveFormat def)
            <*> v .:? "python_interpreter"  .!= defaultPythonInterpreter
            <*> v .:? "matplotlib"   .!= def

    parseJSON _ = fail "Could not parse the configuration"

-- | Default interpreter should be Python 3, which has a different
-- name on Windows ("python") vs Unix ("python3")
defaultPythonInterpreter :: String
#if defined(mingw32_HOST_OS)
defaultPythonInterpreter = "python"
#else
defaultPythonInterpreter = "python3"
#endif