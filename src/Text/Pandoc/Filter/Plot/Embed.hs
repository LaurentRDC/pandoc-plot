{-# LANGUAGE QuasiQuotes #-}
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

import           Text.Shakespeare.Text    (st)

parseTagsFast :: Text -> [Tag Text]
parseTagsFast = canonicalizeTags . parseTagsOptions parseOptionsFast


inside :: String -> [Tag Text] -> [Tag Text]
inside t = takeWhile (~/= TagClose (pack t)) . dropWhile (~/= TagOpen t [])


htmlHead :: [Tag Text] -> [Tag Text]
htmlHead = inside "head"


htmlBody :: [Tag Text] -> [Tag Text]
htmlBody = inside "body"


headScripts :: [Tag Text] -> [[Tag Text]]
headScripts = sections (~== "<scripts>") . htmlHead


extractPlot :: Text -> Text
extractPlot t = 
    let tags = parseTagsFast t  
    in [st|
<div>
#{mconcat $ renderTags <$> headScripts tags}

#{renderTags $ htmlBody tags}
</div>
|]