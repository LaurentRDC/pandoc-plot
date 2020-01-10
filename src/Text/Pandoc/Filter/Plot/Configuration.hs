{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
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
    , dpiKey
    , inclusionKeys
) where

import Text.Pandoc.Filter.Plot.Types

import           Data.Default.Class              (Default, def)
import           Data.String                     (IsString(..))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Data.Yaml
import           Data.Yaml.Config                (ignoreEnv, loadYamlSettings)


-- | Keys that pandoc-plot will look for in code blocks. 
-- These are only exported for testing purposes.
directoryKey, captionKey, includePathKey, saveFormatKey, withLinksKey, dpiKey :: Text
directoryKey     = "directory"
captionKey       = "caption"
includePathKey   = "include"
saveFormatKey    = "format"
withLinksKey     = "links"
dpiKey           = "dpi"


-- | list of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [Text]
inclusionKeys = [ directoryKey
                , captionKey
                , includePathKey
                , saveFormatKey
                , withLinksKey
                , dpiKey
                ]


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
        , defaultWithLinks_   :: Bool
        , defaultDPI_         :: Int
        , defaultSaveFormat_  :: String
        }

instance FromJSON ConfigPrecursor where
    parseJSON (Object v) =
        ConfigPrecursor
            <$> v .:? directoryKey   .!= (defaultDirectory def)
            <*> v .:? withLinksKey   .!= (defaultWithLinks def)
            <*> v .:? dpiKey         .!= (defaultDPI def)
            <*> v .:? saveFormatKey  .!= (extension $ defaultSaveFormat def)

    parseJSON _ = fail "Could not parse the configuration"

renderConfiguration :: ConfigPrecursor -> IO Configuration
renderConfiguration prec = do
    let saveFormat' = fromString $ defaultSaveFormat_ prec
    return $ Configuration
        { defaultDirectory     = defaultDirectory_ prec
        , defaultSaveFormat    = saveFormat'
        , defaultWithLinks     = defaultWithLinks_ prec
        , defaultDPI           = defaultDPI_ prec
        }