{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}


{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module defines types in use in pandoc-plot
-}

module Text.Pandoc.Filter.Plot.Types where

import           Control.Monad.Reader            (MonadIO)
import           Control.Monad.Reader.Class      (MonadReader)

import           Data.Char              (toLower)
import           Data.Default.Class     (Default, def)
import           Data.Hashable          (Hashable(..))
import           Data.List              (intersperse)
import qualified Data.Map.Strict        as Map
import           Data.Semigroup         (Semigroup(..))
import           Data.String            (IsString(..))
import           Data.Text              (Text)
import           Data.Yaml

import           GHC.Generics           (Generic)

import           Text.Pandoc.Definition (Attr)

class ( RendererConfig c , Monad m , MonadIO m , MonadReader c m) 
      => RendererM c m where
    
    -- | Run the renderer, provided a filepath to a config YAML file.
    run :: Maybe FilePath -> m a -> IO a

    -- Name of the renderer. This is the string which will activate
    -- parsing.
    name :: m Text

    -- Extension for script files. A string without periods, e.g. "py", or "m".
    scriptExtension :: m String

    -- | Save formats supported by this renderer.
    supportedSaveFormats :: m [SaveFormat]

    -- Checks to perform before running a script. If ANY check fails,
    -- the figure is not rendered. This is to prevent, for example,
    -- blocking operations to occur.
    scriptChecks :: m [Script -> CheckResult]
    scriptChecks = return mempty

    -- | Parse code block headers for extra attributes that are specific
    -- to this renderer. By default, no extra attributes are parsed.
    parseExtraAttrs :: Map.Map Text Text -> m (Map.Map Text Text)
    parseExtraAttrs _ = return mempty

    -- | Generate the appropriate command-line command to generate a figure.
    command :: FigureSpec 
            -> FilePath     -- ^ Location of the temporary script
            -> m Text

    -- | Script fragment required to capture a figure.
    capture :: FigureSpec 
            -> FilePath     -- ^ Final location of the figure
            -> m Script

-- | Minimum configuration required to run ANY renderer
class (FromJSON c, Default c) => RendererConfig c where
    defaultDirectory    :: c -> FilePath   -- ^ The default directory where figures will be saved.
    defaultWithSource   :: c -> Bool       -- ^ The default behavior of whether or not to include links to source code and high-res
    defaultDPI          :: c -> Int        -- ^ The default dots-per-inch value for generated figures. Renderers might ignore this.
    defaultSaveFormat   :: c -> SaveFormat -- ^ The default save format of generated figures.
    -- Python-specific
    pythonInterpreter   :: c -> String     -- ^ The default Python interpreter to use for Python-based renderers.
    preamble            :: c -> Script     -- ^ Script preamble that is inserted at the beginning of the code block. This is renderer-specific.


type Script = Text

-- | Possible result of running a script
data ScriptResult
    = ScriptSuccess
    | ScriptChecksFailed String
    | ScriptFailure Int

-- | Result of checking scripts for problems
data CheckResult
    = CheckPassed
    | CheckFailed String
    deriving (Eq)

instance Semigroup CheckResult where
    (<>) CheckPassed a                         = a
    (<>) a CheckPassed                         = a
    (<>) (CheckFailed msg1) (CheckFailed msg2) = CheckFailed (msg1 <> msg2)

instance Monoid CheckResult where
    mempty = CheckPassed

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif


-- | Keys that pandoc-plot will look for in code blocks. 
-- These are only exported for testing purposes.
directoryKey, captionKey, saveFormatKey, withSourceKey, preambleKey, dpiKey, pyInterpreterKey :: Text
directoryKey     = "directory"
captionKey       = "caption"
saveFormatKey    = "format"
withSourceKey    = "source"
preambleKey      = "preamble"
dpiKey           = "dpi"
pyInterpreterKey = "python_interpreter"

-- | list of all keys related to pandoc-plot that
-- can be specified in source material.
inclusionKeys :: [Text]
inclusionKeys = [ directoryKey
                , captionKey
                , saveFormatKey
                , withSourceKey
                , dpiKey
                , pyInterpreterKey
                ]


-- | Datatype containing all parameters required to run pandoc-plot.
--
-- It is assumed that once a @FigureSpec@ has been created, no configuration
-- can overload it; hence, a @FigureSpec@ completely encodes a particular figure.
data FigureSpec = FigureSpec
    { caption        :: Text           -- ^ Figure caption.
    , withSource      :: Bool          -- ^ Append link to source code in caption.
    , script         :: Script         -- ^ Source code for the figure.
    , saveFormat     :: SaveFormat     -- ^ Save format of the figure.
    , directory      :: FilePath       -- ^ Directory where to save the file.
    , dpi            :: Int            -- ^ Dots-per-inch of figure.
    , extraAttrs     :: [(Text, Text)] -- ^ Renderer-specific extra attributes.
    , blockAttrs     :: Attr           -- ^ Attributes not related to @pandoc-plot@ will be propagated.
    } deriving Generic

instance Hashable FigureSpec -- From Generic

data BaseConfig = BaseConfig
    { bdefaultDirectory    :: FilePath   -- ^ The default directory where figures will be saved.
    , bdefaultWithSource    :: Bool       -- ^ The default behavior of whether or not to include links to source code and high-res
    , bdefaultDPI          :: Int        -- ^ The default dots-per-inch value for generated figures. Renderers might ignore this.
    , bdefaultSaveFormat   :: SaveFormat -- ^ The default save format of generated figures.
    -- Python-specific
    , bpythonInterpreter   :: String
    } deriving (Generic)

instance RendererConfig BaseConfig where
    defaultDirectory    = bdefaultDirectory
    defaultWithSource   = bdefaultWithSource    
    defaultDPI          = bdefaultDPI    
    defaultSaveFormat   = bdefaultSaveFormat
    pythonInterpreter   = bpythonInterpreter
    -- Base config has trivial preamble
    preamble            = const mempty

instance Default BaseConfig where
    def = BaseConfig
        { bdefaultDirectory   = "generated/"
        , bdefaultDPI         = 80
        , bdefaultWithSource  = False
        , bdefaultSaveFormat  = PNG
        , bpythonInterpreter  = defaultPythonInterpreter
    }

-- | Generated figure file format supported by pandoc-plot.
-- Note: all formats are supported by Matplotlib, but not all
-- formats are supported by Plotly
data SaveFormat
    = PNG
    | PDF
    | SVG
    | JPG
    | EPS
    | GIF
    | TIF
    | WEBP
    deriving (Bounded, Enum, Eq, Show, Generic)

instance Hashable SaveFormat -- From Generic
instance FromJSON SaveFormat -- from Generic


instance IsString SaveFormat where
    -- | An error is thrown if the save format cannot be parsed.
    fromString s
        | s `elem` ["png", "PNG", ".png"] = PNG
        | s `elem` ["pdf", "PDF", ".pdf"] = PDF
        | s `elem` ["svg", "SVG", ".svg"] = SVG
        | s `elem` ["eps", "EPS", ".eps"] = EPS
        | s `elem` ["gif", "GIF", ".gif"] = GIF
        | s `elem` ["jpg", "jpeg", "JPG", "JPEG", ".jpg", ".jpeg"] = JPG
        | s `elem` ["tif", "tiff", "TIF", "TIFF", ".tif", ".tiff"] = TIF
        | s `elem` ["webp", "WEBP", ".webp"] = WEBP
        | otherwise = error $ 
                mconcat [ s
                        , " is not one of valid save format : "
                        , mconcat $ intersperse ", " $ show <$> saveFormats
                        ]
        where
            saveFormats =  (enumFromTo minBound maxBound) :: [SaveFormat]

-- | Save format file extension
extension :: SaveFormat -> String
extension fmt = mconcat [".", fmap toLower . show $ fmt]

-- | Default interpreter should be Python 3, which has a different
-- name on Windows ("python") vs Unix ("python3")
defaultPythonInterpreter :: String
#if defined(mingw32_HOST_OS)
defaultPythonInterpreter = "python"
#else
defaultPythonInterpreter = "python3"
#endif

instance FromJSON BaseConfig where
    parseJSON (Object v) =
        BaseConfig 
            <$> v .:? directoryKey  .!= (defaultDirectory d)
            <*> v .:? withSourceKey .!= (defaultWithSource d)
            <*> v .:? dpiKey        .!= (defaultDPI d)
            <*> v .:? saveFormatKey .!= (defaultSaveFormat d)
            <*> v .:? pyInterpreterKey .!= (pythonInterpreter d)
        where
            d = (def::BaseConfig)

    parseJSON _ = fail "Could not parse the configuration"