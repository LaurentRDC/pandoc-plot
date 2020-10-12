{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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
    , toFigure
) where

import           Data.Default                      (def)
import           Data.List                         (nub)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, pack)
import qualified Data.Text.IO                      as T

import           System.FilePath                   (replaceExtension)

import           Text.HTML.TagSoup

import           Text.Pandoc.Builder               (fromList, imageWith, link,
                                                    para, str, toList, Inlines)
import           Text.Pandoc.Class                 (runPure)
import           Text.Pandoc.Definition            (Pandoc(..), Block (..), Format, Attr)
import           Text.Pandoc.Error                 (handleError)
import           Text.Pandoc.Writers.HTML          (writeHtml5String)

import           Text.Pandoc.Filter.Plot.Parse     (captionReader)
import           Text.Pandoc.Filter.Plot.Monad
import           Text.Pandoc.Filter.Plot.Scripting (figurePath)

import           Text.Shakespeare.Text             (st)


-- | Convert a @FigureSpec@ to a Pandoc figure component.
-- Note that the script to generate figure files must still
-- be run in another function.
toFigure :: Format       -- ^ text format of the caption
         -> FigureSpec 
         -> PlotM Block
toFigure fmt spec = do
    target <- figurePath spec
    sourceLabel <- asksConfig sourceCodeLabel -- Allow the possibility for non-english labels
    let srcLink = link (pack $ replaceExtension target ".txt") mempty (str sourceLabel)
        attrs'       = blockAttrs spec
        withSource'  = withSource spec
        captionText  = fromList $ fromMaybe mempty (captionReader fmt $ caption spec)
        captionLinks = mconcat [" (", srcLink, ")"]
        caption'     = if withSource' then captionText <> captionLinks else captionText
    builder attrs' target caption'
    where
        builder      = if saveFormat spec == HTML
                        then interactiveBlock
                        else figure


figure :: Attr
       -> FilePath
       -> Inlines
       -> PlotM Block
-- To render images as figures with captions, the target title
-- must be "fig:"
-- Janky? yes
figure as fp caption' = return . head . toList . para $ 
                        imageWith as (pack fp) "fig:" caption'


interactiveBlock :: Attr
                 -> FilePath
                 -> Inlines
                 -> PlotM Block
interactiveBlock _ fp caption' = do
    -- TODO: should we instead include the scripts in the "include-after"
    --       template variable?
    --       See https://github.com/jgm/pandoc/issues/6582
    htmlpage <- liftIO $ T.readFile fp
    renderedCaption <- writeHtml caption'
    return $ RawBlock "html5" [st|
<figure>
    <div>
    #{extractPlot htmlpage}
    </div>
    <figcaption>#{renderedCaption}</figcaption>
</figure>
    |]


-- | Convert Pandoc inlines to html
writeHtml :: Inlines -> PlotM Text
writeHtml is = liftIO $ handleError $ runPure $ writeHtml5String def document
    where
        document = Pandoc mempty [Para . toList $ is]


-- | Extract the plot-relevant content from inside of a full HTML document.
-- Scripts contained in the <head> tag are extracted, as well as the entirety of the
-- <body> tag.
extractPlot :: Text -> Text
extractPlot t = let tags = canonicalizeTags $ parseTagsOptions parseOptionsFast t  
                    extracted = headScripts tags <> [inside "body" $ tags]
                in mconcat $ renderTags <$> (deferScripts <$> extracted)
    where
        headScripts = partitions (~== ("<script>"::String)) . inside "head"


-- | Get content inside a tag, e.g. /inside "body"/ returns all tags
-- between /<body>/ and /</body>/
inside :: Text -> [Tag Text] -> [Tag Text]
inside t = init . tail . tgs
    where
        tgs = takeWhile (~/= TagClose t) . dropWhile (~/= TagOpen t [])


data ScriptTag = InlineScript [Attribute Text]
               | ExternalScript [Attribute Text]


fromTag :: Tag Text -> Maybe ScriptTag
fromTag (TagOpen "script" attrs) = 
    Just $ if "src" `elem` (fst . unzip $ attrs) 
        then ExternalScript attrs
        else InlineScript attrs
fromTag _ = Nothing


toTag :: ScriptTag -> Tag Text
toTag (InlineScript t)   = TagOpen "script" t
toTag (ExternalScript t) = TagOpen "script" t


deferScript :: ScriptTag -> ScriptTag
deferScript (InlineScript   attrs) = InlineScript   $ nub $ attrs <> [("type", "module")]
deferScript (ExternalScript attrs) = ExternalScript $ nub $ attrs <> [("defer", mempty)]


-- | Replace /<script src=...>/ tags with /<script src=... defer>/,
-- and inline scripts as /<script type="module">/.
-- This makes scripts execute only after HTML parsing has finished.
deferScripts :: [Tag Text] -> [Tag Text]
deferScripts = fmap (\t -> maybe t (toTag . deferScript) (fromTag t))