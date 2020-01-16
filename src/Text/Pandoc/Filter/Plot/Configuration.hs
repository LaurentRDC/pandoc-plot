{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Reading configuration from file
-}

module Text.Pandoc.Filter.Plot.Configuration (
    configuration
) where

import           Data.Default.Class     (Default, def)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, pack)
import qualified Data.Text.IO           as TIO
import           Data.Yaml
import           Data.Yaml.Config       (ignoreEnv, loadYamlSettings)

import Text.Pandoc.Filter.Plot.Types

-- | Read configuration from a YAML file. The
-- keys are exactly the same as for code blocks.
--
-- If a key is either not present or unreadable, its value will be set
-- to the default value.
configuration :: FilePath -> IO Configuration
configuration fp = (loadYamlSettings [fp] [] ignoreEnv) >>= renderConfig


-- We define a precursor type because preambles are best specified as file paths,
-- but we want to read those files before building a full
-- @Configuration@ value.
data ConfigPrecursor = ConfigPrecursor
    { _defaultDirectory  :: FilePath   -- ^ The default directory where figures will be saved.
    , _defaultWithSource :: Bool       -- ^ The default behavior of whether or not to include links to source code and high-res
    , _defaultDPI        :: Int        -- ^ The default dots-per-inch value for generated figures. Renderers might ignore this.
    , _defaultSaveFormat :: SaveFormat -- ^ The default save format of generated figures.
    , _pythonInterpreter :: String     -- ^ The default Python interpreter to use for Python-based renderers.
    
    , _matplotlibPrec    :: MatplotlibPrecursor
    , _matlabPrec        :: MatlabPrecursor
    , _plotlyPythonPrec  :: PlotlyPythonPrecursor
    , _mathematicaPrec   :: MathematicaPrecursor
    , _octavePrec        :: OctavePrecursor
    }


-- Separate YAML clauses have their own types.
data MatplotlibPrecursor = MatplotlibPrecursor
        { _matplotlibPreamble    :: Maybe FilePath
        , _matplotlibTightBBox   :: Bool
        , _matplotlibTransparent :: Bool
        }
data MatlabPrecursor        = MatlabPrecursor {_matlabPreamble :: Maybe FilePath}
data PlotlyPythonPrecursor  = PlotlyPythonPrecursor {_plotlyPythonPreamble :: Maybe FilePath}
data MathematicaPrecursor   = MathematicaPrecursor {_mathematicaPreamble :: Maybe FilePath}
data OctavePrecursor        = OctavePrecursor {_octavePreamble :: Maybe FilePath}


instance Default MatplotlibPrecursor where
    def = MatplotlibPrecursor Nothing (matplotlibTightBBox def) (matplotlibTransparent def)

instance Default MatlabPrecursor        where def = MatlabPrecursor Nothing
instance Default PlotlyPythonPrecursor  where def = PlotlyPythonPrecursor Nothing
instance Default MathematicaPrecursor   where def = MathematicaPrecursor Nothing
instance Default OctavePrecursor        where def = OctavePrecursor Nothing

instance FromJSON MatplotlibPrecursor where
    parseJSON (Object v) = 
        MatplotlibPrecursor
            <$> v .:? (tshow MatplotlibPreambleK)
            <*> v .:? (tshow MatplotlibTightBBoxK)   .!= (_matplotlibTightBBox def) 
            <*> v .:? (tshow MatplotlibTransparentK) .!= (_matplotlibTransparent def)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Matplotlib, " configuration."]

instance FromJSON MatlabPrecursor where
    parseJSON (Object v) = MatlabPrecursor <$> v .:? (tshow MatlabPreambleK)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Matlab, " configuration."]

instance FromJSON PlotlyPythonPrecursor where
    parseJSON (Object v) = PlotlyPythonPrecursor <$> v .:? (tshow PlotlyPythonPreambleK)
    parseJSON _ = fail $ mconcat ["Could not parse ", show PlotlyPython, " configuration."]

instance FromJSON MathematicaPrecursor where
    parseJSON (Object v) = MathematicaPrecursor <$> v .:? (tshow MathematicaPreambleK)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Mathematica, " configuration."]

instance FromJSON OctavePrecursor where
    parseJSON (Object v) = OctavePrecursor <$> v .:? (tshow OctavePreambleK)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Octave, " configuration."]



instance FromJSON ConfigPrecursor where
    parseJSON (Object v) = do
        
        _defaultDirectory  <- v .:? (tshow DirectoryK)     .!= (defaultDirectory def)
        _defaultWithSource <- v .:? (tshow WithSourceK)    .!= (defaultWithSource def)
        _defaultDPI        <- v .:? (tshow DpiK)           .!= (defaultDPI def)
        _defaultSaveFormat <- v .:? (tshow SaveFormatK)    .!= (defaultSaveFormat def)
        _pythonInterpreter <- v .:? (tshow PyInterpreterK) .!= (pythonInterpreter def)

        _matplotlibPrec    <- v .:? (cls Matplotlib)       .!= def
        _matlabPrec        <- v .:? (cls Matlab)           .!= def
        _plotlyPythonPrec  <- v .:? (cls PlotlyPython)     .!= def
        _mathematicaPrec   <- v .:? (cls Mathematica)      .!= def
        _octavePrec        <- v .:? (cls Octave)           .!= def

        return $ ConfigPrecursor{..}
    parseJSON _          = fail "Could not parse basic configuration."


renderConfig :: ConfigPrecursor -> IO Configuration
renderConfig ConfigPrecursor{..} = do
    let defaultDirectory  = _defaultDirectory
        defaultWithSource = _defaultWithSource
        defaultDPI        = _defaultDPI
        defaultSaveFormat = _defaultSaveFormat
        pythonInterpreter = _pythonInterpreter

        matplotlibTightBBox   = _matplotlibTightBBox _matplotlibPrec
        matplotlibTransparent = _matplotlibTransparent _matplotlibPrec
    
    matplotlibPreamble   <- readPreamble (_matplotlibPreamble _matplotlibPrec)
    matlabPreamble       <- readPreamble (_matlabPreamble _matlabPrec)
    plotlyPythonPreamble <- readPreamble (_plotlyPythonPreamble _plotlyPythonPrec)
    mathematicaPreamble  <- readPreamble (_mathematicaPreamble _mathematicaPrec)
    octavePreamble       <- readPreamble (_octavePreamble _octavePrec)

    return Configuration{..}
    where
        readPreamble fp = fromMaybe mempty $ TIO.readFile <$> fp



tshow :: Show a => a -> Text
tshow = pack . show

-- data Configuration = Configuration
--     { defaultDirectory      :: FilePath   -- ^ The default directory where figures will be saved.
--     , defaultWithSource     :: Bool       -- ^ The default behavior of whether or not to include links to source code and high-res
--     , defaultDPI            :: Int        -- ^ The default dots-per-inch value for generated figures. Renderers might ignore this.
--     , defaultSaveFormat     :: SaveFormat -- ^ The default save format of generated figures.
--     , pythonInterpreter     :: String     -- ^ The default Python interpreter to use for Python-based renderers.
--     -- Default preambles
--     , matplotlibPreamble    :: Script
--     , plotlyPythonPreamble  :: Script
--     , matlabPreamble        :: Script
--     , mathematicaPreamble   :: Script
--     , octavePreamble        :: Script
--     -- Toolkit-specific options
--     , matplotlibTightBBox   :: Bool
--     , matplotlibTransparent :: Bool
--     }

