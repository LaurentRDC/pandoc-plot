{-# LANGUAGE TemplateHaskell #-}

import Criterion.Main
  ( bench,
    bgroup,
    defaultMain,
    envWithCleanup,
    nfIO,
  )
import MatplotlibGallery
  ( galleryItem1,
    galleryItem2,
    galleryItem3,
    galleryItem4,
  )
import Text.Pandoc.Definition (Block (CodeBlock), Pandoc (..))
import Text.Pandoc.Filter.Plot
  ( Configuration (logSink, logVerbosity),
    LogSink (StdErr),
    Script,
    Toolkit (Matplotlib),
    Verbosity (Silent),
    cleanOutputDirs,
    defaultConfiguration,
    plotFilter,
  )
import Text.Pandoc.Filter.Plot.Internal (cls)

main :: IO ()
main =
  defaultMain
    [ envWithCleanup (return ()) (\_ -> cleanupEnv) $ \_ ->
        bgroup
          "main"
          [ bench "filter" $ nfIO (plotFilter plotConfig Nothing benchDoc)
          ]
    ]

plotConfig :: Configuration
plotConfig = defaultConfiguration {logVerbosity = Silent, logSink = StdErr}

cleanupEnv :: IO ()
cleanupEnv = cleanOutputDirs plotConfig benchDoc >> return ()

codeBlock :: Script -> Block
codeBlock = CodeBlock (mempty, [cls Matplotlib], mempty)

benchDoc :: Pandoc
benchDoc =
  Pandoc
    mempty
    [ codeBlock $(galleryItem1),
      codeBlock $(galleryItem2),
      codeBlock $(galleryItem3),
      codeBlock $(galleryItem4)
    ]
