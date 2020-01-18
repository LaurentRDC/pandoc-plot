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
    
    , _matplotlibPrec    :: MatplotlibPrecursor
    , _matlabPrec        :: MatlabPrecursor
    , _plotlyPythonPrec  :: PlotlyPythonPrecursor
    , _mathematicaPrec   :: MathematicaPrecursor
    , _octavePrec        :: OctavePrecursor
    }

instance Default ConfigPrecursor where
    def = ConfigPrecursor
        { _defaultDirectory  = defaultDirectory def
        , _defaultWithSource = defaultWithSource def
        , _defaultDPI        = defaultDPI def
        , _defaultSaveFormat = defaultSaveFormat def
        
        , _matplotlibPrec    = def
        , _matlabPrec        = def
        , _plotlyPythonPrec  = def
        , _mathematicaPrec   = def
        , _octavePrec        = def
        }


-- Separate YAML clauses have their own types.
data MatplotlibPrecursor = MatplotlibPrecursor
        { _matplotlibPreamble    :: Maybe FilePath
        , _matplotlibTightBBox   :: Bool
        , _matplotlibTransparent :: Bool
        , _matplotlibExe         :: FilePath
        }
data MatlabPrecursor        = MatlabPrecursor {_matlabPreamble :: Maybe FilePath, _matlabExe :: FilePath}
data PlotlyPythonPrecursor  = PlotlyPythonPrecursor {_plotlyPythonPreamble :: Maybe FilePath, _plotlyPythonExe :: FilePath}
data MathematicaPrecursor   = MathematicaPrecursor {_mathematicaPreamble :: Maybe FilePath, _mathematicaExe :: FilePath}
data OctavePrecursor        = OctavePrecursor {_octavePreamble :: Maybe FilePath, _octaveExe :: FilePath}


instance Default MatplotlibPrecursor where
    def = MatplotlibPrecursor Nothing (matplotlibTightBBox def) (matplotlibTransparent def) (matplotlibExe def)

instance Default MatlabPrecursor        where def = MatlabPrecursor Nothing (matlabExe def)
instance Default PlotlyPythonPrecursor  where def = PlotlyPythonPrecursor Nothing (plotlyPythonExe def)
instance Default MathematicaPrecursor   where def = MathematicaPrecursor Nothing (mathematicaExe def)
instance Default OctavePrecursor        where def = OctavePrecursor Nothing (octaveExe def)

instance FromJSON MatplotlibPrecursor where
    parseJSON (Object v) = 
        MatplotlibPrecursor
            <$> v .:? (tshow MatplotlibPreambleK)
            <*> v .:? (tshow MatplotlibTightBBoxK)   .!= (matplotlibTightBBox def) 
            <*> v .:? (tshow MatplotlibTransparentK) .!= (matplotlibTransparent def)
            <*> v .:? (tshow MatplotlibExecutableK)  .!= (matplotlibExe def)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Matplotlib, " configuration."]

instance FromJSON MatlabPrecursor where
    parseJSON (Object v) = MatlabPrecursor <$> v .:? (tshow MatlabPreambleK) <*> v .:? (tshow MatlabExecutableK) .!= (matlabExe def)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Matlab, " configuration."]

instance FromJSON PlotlyPythonPrecursor where
    parseJSON (Object v) = PlotlyPythonPrecursor <$> v .:? (tshow PlotlyPythonPreambleK) <*> v .:? (tshow PlotlyPythonExecutableK) .!= (plotlyPythonExe def)
    parseJSON _ = fail $ mconcat ["Could not parse ", show PlotlyPython, " configuration."]

instance FromJSON MathematicaPrecursor where
    parseJSON (Object v) = MathematicaPrecursor <$> v .:? (tshow MathematicaPreambleK) <*> v .:? (tshow MathematicaExecutableK) .!= (mathematicaExe def)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Mathematica, " configuration."]

instance FromJSON OctavePrecursor where
    parseJSON (Object v) = OctavePrecursor <$> v .:? (tshow OctavePreambleK) <*> v .:? (tshow OctaveExecutableK) .!= (octaveExe def)
    parseJSON _ = fail $ mconcat ["Could not parse ", show Octave, " configuration."]


instance FromJSON ConfigPrecursor where
    parseJSON (Null) = return def -- In case of empty file
    parseJSON (Object v) = do
        
        _defaultDirectory  <- v .:? (tshow DirectoryK)     .!= (defaultDirectory def)
        _defaultWithSource <- v .:? (tshow WithSourceK)    .!= (defaultWithSource def)
        _defaultDPI        <- v .:? (tshow DpiK)           .!= (defaultDPI def)
        _defaultSaveFormat <- v .:? (tshow SaveFormatK)    .!= (_defaultSaveFormat def)

        _matplotlibPrec    <- v .:? (cls Matplotlib)       .!= def
        _matlabPrec        <- v .:? (cls Matlab)           .!= def
        _plotlyPythonPrec  <- v .:? (cls PlotlyPython)     .!= def
        _mathematicaPrec   <- v .:? (cls Mathematica)      .!= def
        _octavePrec        <- v .:? (cls Octave)           .!= def

        return $ ConfigPrecursor{..}
    parseJSON _          = fail "Could not parse configuration."


renderConfig :: ConfigPrecursor -> IO Configuration
renderConfig ConfigPrecursor{..} = do
    let defaultDirectory  = _defaultDirectory
        defaultWithSource = _defaultWithSource
        defaultDPI        = _defaultDPI
        defaultSaveFormat = _defaultSaveFormat

        matplotlibTightBBox   = _matplotlibTightBBox _matplotlibPrec
        matplotlibTransparent = _matplotlibTransparent _matplotlibPrec

        matplotlibExe   = _matplotlibExe _matplotlibPrec
        matlabExe       = _matlabExe _matlabPrec
        plotlyPythonExe = _plotlyPythonExe _plotlyPythonPrec
        mathematicaExe  = _mathematicaExe _mathematicaPrec
        octaveExe       = _octaveExe _octavePrec
    
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