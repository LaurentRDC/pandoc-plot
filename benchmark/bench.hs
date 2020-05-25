{-# LANGUAGE TemplateHaskell #-}

import Criterion.Main

import Text.Pandoc.Definition
import Text.Pandoc.Filter.Plot
import Text.Pandoc.Filter.Plot.Internal

import MatplotlibGallery (galleryItem1, galleryItem2)

main :: IO ()
main = 
    defaultMain [
        envWithCleanup (return ()) (\_ -> cleanupEnv) $ \_ -> 
            bgroup "main" [
                  bench "filter" $ nfIO (plotTransform defaultConfiguration benchDoc)
                ]
    ]

cleanupEnv :: IO ()
cleanupEnv = cleanOutputDirs defaultConfiguration benchDoc >> return ()


codeBlock :: Script -> Block
codeBlock = CodeBlock (mempty, [cls Matplotlib], mempty)


benchDoc :: Pandoc
benchDoc = Pandoc mempty [ codeBlock $(galleryItem1)
                         , codeBlock $(galleryItem2)
                         ]