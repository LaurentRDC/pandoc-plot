{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
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

    -- * Concurrent execution
    mapConcurrentlyN,

    -- * Running external commands
    runCommand,
    withPrependedPath,

    -- * Halting pandoc-plot
    throwStrictError,

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

    -- * Base types
    module Text.Pandoc.Filter.Plot.Monad.Types,
  )
where

import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.QSemN
  ( QSemN,
    newQSemN,
    signalQSemN,
    waitQSemN,
  )
import Control.Exception.Lifted (bracket, bracket_)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT (runReaderT),
    asks,
  )
import Control.Monad.State.Strict
  ( MonadState (get, put),
    StateT,
    evalStateT,
  )
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
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
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.Process.Typed
  ( byteStringInput,
    byteStringOutput,
    nullStream,
    readProcessStderr,
    setStderr,
    setStdin,
    setStdout,
    setWorkingDir,
    shell,
  )
import Text.Pandoc.Definition (Format (..))
import Text.Pandoc.Filter.Plot.Monad.Logging
  ( LogSink (..),
    Logger,
    MonadLogger (..),
    Verbosity (..),
    debug,
    err,
    info,
    strict,
    terminateLogging,
    warning,
    withLogger,
  )
import Text.Pandoc.Filter.Plot.Monad.Types
import Prelude hiding (fst, log, snd)

-- | pandoc-plot monad
type PlotM = StateT PlotState (ReaderT RuntimeEnv IO)

instance MonadLogger PlotM where
  askLogger = asks envLogger

data RuntimeEnv = RuntimeEnv
  { envFormat :: Maybe Format, -- pandoc output format
    envConfig :: Configuration,
    envLogger :: Logger,
    envCWD :: FilePath
  }

-- | Get access to configuration within the @PlotM@ monad.
asksConfig :: (Configuration -> a) -> PlotM a
asksConfig f = asks (f . envConfig)

-- | Evaluate a @PlotM@ action.
runPlotM :: Maybe Format -> Configuration -> PlotM a -> IO a
runPlotM fmt conf v = do
  cwd <- getCurrentDirectory
  st <-
    PlotState <$> newMVar mempty
      <*> newMVar mempty
  let verbosity = logVerbosity conf
      sink = logSink conf
  withLogger verbosity sink $
    \logger -> runReaderT (evalStateT v st) (RuntimeEnv fmt conf logger cwd)

-- | maps a function, performing at most @N@ actions concurrently.
mapConcurrentlyN :: Traversable t => Int -> (a -> PlotM b) -> t a -> PlotM (t b)
mapConcurrentlyN n f xs = do
  -- Emulating a pool of processes with locked access
  sem <- liftIO $ newQSemN n
  mapConcurrently (with sem . f) xs
  where
    with :: QSemN -> PlotM a -> PlotM a
    with s = bracket_ (liftIO $ waitQSemN s 1) (liftIO $ signalQSemN s 1)

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
        -- For Julia specifically, if the line below is not there (`setStdin (byteStringInput "")`),
        -- the following error is thrown on Windows:
        --    ERROR: error initializing stdin in uv_dup:
        --           Unknown system error 50 (Unknown system error 50 50)
        setStdin (byteStringInput "") $
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

-- | Prepend a directory to the PATH environment variable for the duration
-- of a computation.
--
-- This function is exception-safe; even if an exception happens during the
-- computation, the PATH environment variable will be reverted back to
-- its initial value.
withPrependedPath :: FilePath -> PlotM a -> PlotM a
withPrependedPath dir f = do
  pathVar <- liftIO $ getEnv "PATH"
  let pathVarPrepended = mconcat [dir, ";", pathVar]
  bracket
    (liftIO $ setEnv "PATH" pathVarPrepended)
    (\_ -> liftIO $ setEnv "PATH" pathVar)
    (const f)

-- | Throw an error that halts the execution of pandoc-plot due to a strict-mode.
throwStrictError :: Text -> PlotM ()
throwStrictError msg = do
  strict msg
  logger <- askLogger
  liftIO $ terminateLogging logger >> exitFailure

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
        findExecutable name <&> fmap exeFromPath
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
    exeSelector PlantUML = asksConfig plantumlExe

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
    -- | Whether to halt pandoc-plot when an error is encountered or not.
    strictMode :: !Bool,
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
    -- | The default preamble script for the PlantUML toolkit.
    plantumlPreamble :: !Script,
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
    -- | The executable to use to generate figures using the PlantUML toolkit.
    plantumlExe :: !FilePath,
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
    -- | Command-line arguments to pass to the interpreter for the plantUML toolkit.
    plantumlCmdArgs :: !Text,
    -- | Whether or not to make Matplotlib figures tight by default.
    matplotlibTightBBox :: !Bool,
    -- | Whether or not to make Matplotlib figures transparent by default.
    matplotlibTransparent :: !Bool
  }
  deriving (Eq, Show)
