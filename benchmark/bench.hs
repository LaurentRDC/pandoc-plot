{-# LANGUAGE TemplateHaskell #-}

import Criterion.Main

import Text.Pandoc.Definition
import Text.Pandoc.Filter.Plot
import Text.Pandoc.Filter.Plot.Internal

import MatplotlibGallery ( galleryItem1, galleryItem2
                         , galleryItem3, galleryItem4
                         )

main :: IO ()
main = 
    defaultMain [
        envWithCleanup (return ()) (\_ -> cleanupEnv) $ \_ -> 
            bgroup "main" [
                    bench "filter-async" $ nfIO (plotTransform plotConfig benchDoc)
                  , bench "filter"       $ nfIO (makePlot plotConfig benchDoc)
                ]
    ]

plotConfig :: Configuration
plotConfig = defaultConfiguration {logVerbosity=Silent, logSink =StdErr}

cleanupEnv :: IO ()
cleanupEnv = cleanOutputDirs plotConfig benchDoc >> return ()


codeBlock :: Script -> Block
codeBlock = CodeBlock (mempty, [cls Matplotlib], mempty)


benchDoc :: Pandoc
benchDoc = Pandoc mempty [ codeBlock $(galleryItem1)
                         , codeBlock $(galleryItem2)
                         , codeBlock $(galleryItem3)
                         , codeBlock $(galleryItem4)
                         ]