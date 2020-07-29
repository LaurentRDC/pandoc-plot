{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Embedding HTML content
-}

module Text.Pandoc.Filter.Plot.Embed (
    extractPlot
) where

import           Data.Text                (Text, pack)

import           Text.HTML.TagSoup

-- | Extract the plot-relevant content from inside of a full HTML document.
-- Scripts contained in the <head> tag are extracted, as well as the entirety of the
-- <body> tag.
extractPlot :: Text -> Text
extractPlot t = let tags = parseTagsFast t  
                in mconcat $ renderTags <$> (headScripts tags <> [htmlBody tags])


parseTagsFast :: Text -> [Tag Text]
parseTagsFast = canonicalizeTags . parseTagsOptions parseOptionsFast


inside :: String -> [Tag Text] -> [Tag Text]
inside t = init . tail . tgs
    where
        tgs = takeWhile (~/= TagClose (pack t)) . dropWhile (~/= TagOpen t [])


htmlHead :: [Tag Text] -> [Tag Text]
htmlHead = inside "head"


htmlBody :: [Tag Text] -> [Tag Text]
htmlBody = inside "body"


headScripts :: [Tag Text] -> [[Tag Text]]
headScripts = partitions (~== "<script>") . htmlHead