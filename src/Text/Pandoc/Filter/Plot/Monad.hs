{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2020
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- This module defines the @PlotM@ monad and related capabilities.
module Text.Pandoc.Filter.Plot.Monad
  ( Configuration (..),
    PlotM,
    RuntimeEnv (..),
    PlotState (..),
    runPlotM,

    -- * Running external commands
    runCommand,

    -- * Getting file hashes
    fileHash,

    -- * Getting executables
    executable,

    -- * Logging
    Verbosity (..),
    LogSink (..),
    debug,
    err,
    warning,
    info,

    -- * Lifting and other monadic operations
    liftIO,
    ask,
    asks,
    asksConfig,
    silence,

    -- * Base types
    module Text.Pandoc.Filter.Plot.Monad.Types,
  )
where

import Control.Concurrent.Chan (writeChan)
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.ByteString.Lazy (toStrict)
import Data.Hashable (hash)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory
  ( doesFileExist,
    findExecutable,
    getCurrentDirectory,
    getModificationTime,
  )
import System.Exit (ExitCode (..))
import System.Process.Typed
  ( byteStringOutput,
    nullStream,
    readProcessStderr,
    setStderr,
    setStdout,
    setWorkingDir,
    shell,
  )
import Text.Pandoc.Definition (Format (..))
import Text.Pandoc.Filter.Plot.Monad.Logging as Log
import Text.Pandoc.Filter.Plot.Monad.Types
import Prelude hiding (fst, log, snd)

-- | pandoc-plot monad
type PlotM a = StateT PlotState (ReaderT RuntimeEnv IO) a

data RuntimeEnv = RuntimeEnv
  { envConfig :: Configuration,
    envLogger :: Logger,
    envCWD :: FilePath
  }

-- | Modify the runtime environment to be silent.
silence :: PlotM a -> PlotM a
silence = local (\(RuntimeEnv c l d) -> RuntimeEnv c l {lVerbosity = Silent} d)

-- | Get access to configuration within the @PlotM@ monad.
asksConfig :: (Configuration -> a) -> PlotM a
asksConfig f = asks (f . envConfig)

-- | Evaluate a @PlotM@ action.
runPlotM :: Configuration -> PlotM a -> IO a
runPlotM conf v = do
  cwd <- getCurrentDirectory
  st <-
    PlotState <$> newMVar mempty
      <*> newMVar mempty
  let verbosity = logVerbosity conf
      sink = logSink conf
  withLogger verbosity sink $
    \logger -> runReaderT (evalStateT v st) (RuntimeEnv conf logger cwd)

debug, err, warning, info :: Text -> PlotM ()
debug = log "DEBUG | " Debug
err = log "ERROR | " Error
warning = log "WARN  | " Warning
info = log "INFO  | " Info

-- | General purpose logging.
log ::
  -- | Header.
  Text ->
  -- | Verbosity of the message.
  Verbosity ->
  -- | Message (can be multiple lines).
  Text ->
  PlotM ()
log h v t = do
  logger <- asks envLogger
  when (v >= lVerbosity logger) $
    liftIO $ do
      forM_ (T.lines t) $ \l -> writeChan (lChannel logger) (Just (h <> l <> "\n"))

-- | Run a command within the @PlotM@ monad. Stderr stream
-- is read and decoded, while Stdout is ignored.
-- Logging happens at the debug level if the command succeeds, or at
-- the error level if it does not succeed.
runCommand ::
  FilePath -> -- Directory from which to run the command
  Text -> -- Command to run, including executable
  PlotM (ExitCode, Text)
runCommand wordir command = do
  (ec, processOutput') <-
    liftIO $
      readProcessStderr $
        setStdout nullStream $
          setStderr byteStringOutput $
            setWorkingDir wordir $
              shell (unpack command)
  let processOutput = decodeUtf8With lenientDecode $ toStrict processOutput'
      logFunc =
        if ec == ExitSuccess
          then debug
          else err
      message =
        T.unlines
          [ "Running command",
            "    " <> command,
            "ended with exit code " <> (pack . show $ ec)
          ]
      errorMessage =
        if processOutput == mempty
          then mempty
          else
            T.unlines
              [ "*******",
                processOutput,
                "*******"
              ]

  logFunc $ message <> errorMessage
  return (ec, processOutput)

-- Plot state is used for caching.
-- One part consists of a map of filepaths to hashes
-- This allows multiple plots to depend on the same file/directory, and the file hashes
-- will only be calculated once. This is OK because pandoc-plot will not run for long.
-- We note that because figures are rendered possibly in parallel, access to
-- the state must be synchronized; otherwise, each thread might compute its own
-- hashes.
-- The other part is comprised of a map of toolkits to renderers (possibly missing)
-- This means that checking if renderers are available will only be done once.
type FileHash = Word

data PlotState
  = PlotState
      (MVar (Map FilePath FileHash))
      (MVar (Map Toolkit (Maybe Renderer)))

-- | Get a filehash. If the file hash has been computed before,
-- it is reused. Otherwise, the filehash is calculated and stored.
fileHash :: FilePath -> PlotM FileHash
fileHash path = do
  PlotState varHashes varExes <- get
  hashes <- liftIO $ takeMVar varHashes
  (fh, hashes') <- case M.lookup path hashes of
    Nothing -> do
      debug $ mconcat ["Calculating hash of dependency ", pack path]
      fh <- fileHash' path
      let hashes' = M.insert path fh hashes
      return (fh, hashes')
    Just h -> do
      debug $ mconcat ["Hash of dependency ", pack path, " already calculated."]
      return (h, hashes)
  liftIO $ putMVar varHashes hashes'
  put $ PlotState varHashes varExes
  return fh
  where
    -- As a proxy for the state of a file dependency, we use the modification time
    -- This is much faster than actual file hashing
    fileHash' :: FilePath -> PlotM FileHash
    fileHash' fp = do
      fileExists <- liftIO $ doesFileExist fp
      if fileExists
        then liftIO . fmap (fromIntegral . hash . show) . getModificationTime $ fp
        else err (mconcat ["Dependency ", pack fp, " does not exist."]) >> return 0

-- | Find an executable.
executable :: Toolkit -> PlotM (Maybe Executable)
executable tk =
  exeSelector tk
    >>= \name ->
      liftIO $
        findExecutable name
          >>= return . fmap exeFromPath
  where
    exeSelector Matplotlib = asksConfig matplotlibExe
    exeSelector PlotlyPython = asksConfig plotlyPythonExe
    exeSelector PlotlyR = asksConfig plotlyRExe
    exeSelector Matlab = asksConfig matlabExe
    exeSelector Mathematica = asksConfig mathematicaExe
    exeSelector Octave = asksConfig octaveExe
    exeSelector GGPlot2 = asksConfig ggplot2Exe
    exeSelector GNUPlot = asksConfig gnuplotExe
    exeSelector Graphviz = asksConfig graphvizExe
    exeSelector Bokeh = asksConfig bokehExe
    exeSelector Plotsjl = asksConfig plotsjlExe

-- | The @Configuration@ type holds the default values to use
-- when running pandoc-plot. These values can be overridden in code blocks.
--
-- You can create an instance of the @Configuration@ type from file using the @configuration@ function.
--
-- You can store the path to a configuration file in metadata under the key @plot-configuration@. For example, in Markdown:
--
-- @
--     ---
--     title: My document
--     author: John Doe
--     plot-configuration: path\to\file.yml
--     ---
-- @
--
-- The same can be specified via the command line using Pandoc's @-M@ flag:
--
-- > pandoc --filter pandoc-plot -M plot-configuration="path/to/file.yml" ...
--
-- In this case, use @configurationPathMeta@ to extact the path from @Pandoc@ documents.
data Configuration = Configuration
  { -- | The default directory where figures will be saved.
    defaultDirectory :: !FilePath,
    -- | The default behavior of whether or not to include links to source code and high-res
    defaultWithSource :: !Bool,
    -- | The default dots-per-inch value for generated figures. Renderers might ignore this.
    defaultDPI :: !Int,
    -- | The default save format of generated figures.
    defaultSaveFormat :: !SaveFormat,
    -- | List of files/directories on which all figures depend.
    defaultDependencies :: ![FilePath],
    -- | Caption format, in the same notation as Pandoc format, e.g. "markdown+tex_math_dollars"
    captionFormat :: !Format,
    -- | The text label to which the source code is linked. Change this if you are writing non-english documents.
    sourceCodeLabel :: !Text,
    -- | Level of logging verbosity.
    logVerbosity :: !Verbosity,
    -- | Method of logging, i.e. printing to stderr or file.
    logSink :: !LogSink,
    -- | The default preamble script for the matplotlib toolkit.
    matplotlibPreamble :: !Script,
    -- | The default preamble script for the Plotly/Python toolkit.
    plotlyPythonPreamble :: !Script,
    -- | The default preamble script for the Plotly/R toolkit.
    plotlyRPreamble :: !Script,
    -- | The default preamble script for the MATLAB toolkit.
    matlabPreamble :: !Script,
    -- | The default preamble script for the Mathematica toolkit.
    mathematicaPreamble :: !Script,
    -- | The default preamble script for the GNU Octave toolkit.
    octavePreamble :: !Script,
    -- | The default preamble script for the GGPlot2 toolkit.
    ggplot2Preamble :: !Script,
    -- | The default preamble script for the gnuplot toolkit.
    gnuplotPreamble :: !Script,
    -- | The default preamble script for the Graphviz toolkit.
    graphvizPreamble :: !Script,
    -- | The default preamble script for the Python/Bokeh toolkit.
    bokehPreamble :: !Script,
    -- | The default preamble script for the Julia/Plots.jl toolkit.
    plotsjlPreamble :: !Script,
    -- | The executable to use to generate figures using the matplotlib toolkit.
    matplotlibExe :: !FilePath,
    -- | The executable to use to generate figures using the MATLAB toolkit.
    matlabExe :: !FilePath,
    -- | The executable to use to generate figures using the Plotly/Python toolkit.
    plotlyPythonExe :: !FilePath,
    -- | The executable to use to generate figures using the Plotly/R toolkit.
    plotlyRExe :: !FilePath,
    -- | The executable to use to generate figures using the Mathematica toolkit.
    mathematicaExe :: !FilePath,
    -- | The executable to use to generate figures using the GNU Octave toolkit.
    octaveExe :: !FilePath,
    -- | The executable to use to generate figures using the GGPlot2 toolkit.
    ggplot2Exe :: !FilePath,
    -- | The executable to use to generate figures using the gnuplot toolkit.
    gnuplotExe :: !FilePath,
    -- | The executable to use to generate figures using the Graphviz toolkit.
    graphvizExe :: !FilePath,
    -- | The executable to use to generate figures using the Python/Bokeh toolkit.
    bokehExe :: !FilePath,
    -- | The executable to use to generate figures using the Julia/Plots.jl toolkit.
    plotsjlExe :: !FilePath,
    -- | Command-line arguments to pass to the Python interpreter for the Matplotlib toolkit
    matplotlibCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the MATLAB toolkit.
    matlabCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the Plotly/Python toolkit.
    plotlyPythonCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the Plotly/R toolkit.
    plotlyRCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the Mathematica toolkit.
    mathematicaCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the GNU Octave toolkit.
    octaveCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the GGPlot2 toolkit.
    ggplot2CmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the gnuplot toolkit.
    gnuplotCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the Graphviz toolkit.
    graphvizCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the Python/Bokeh toolkit.
    bokehCmdArgs :: !Text,
    -- | Command-line arguments to pass to the interpreter for the Julia/Plots.jl toolkit.
    plotsjlCmdArgs :: !Text,
    -- | Whether or not to make Matplotlib figures tight by default.
    matplotlibTightBBox :: !Bool,
    -- | Whether or not to make Matplotlib figures transparent by default.
    matplotlibTransparent :: !Bool
  }
  deriving (Eq, Show)
