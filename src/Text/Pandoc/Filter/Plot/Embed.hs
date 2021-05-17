{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - 2021
-- License     : GNU GPL, version 2 or above
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : internal
-- Portability : portable
--
-- Embedding HTML and LaTeX content
module Text.Pandoc.Filter.Plot.Embed
  ( extractPlot,
    toFigure,
  )
where

import Data.Default (def)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Text.HTML.TagSoup
  ( Attribute,
    Tag (TagClose, TagOpen),
    canonicalizeTags,
    parseOptionsFast,
    parseTagsOptions,
    partitions,
    renderTags,
    (~/=),
    (~==),
  )
import Text.Pandoc.Builder
  ( Inlines,
    fromList,
    imageWith,
    link,
    para,
    str,
    toList,
  )
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition (Attr, Block (..), Format, Pandoc (..))
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Filter.Plot.Monad
import Text.Pandoc.Filter.Plot.Parse (captionReader)
import Text.Pandoc.Filter.Plot.Scripting (figurePath, sourceCodePath)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)
import Text.Shakespeare.Text (st)

-- | Convert a @FigureSpec@ to a Pandoc figure component.
-- Note that the script to generate figure files must still
-- be run in another function.
toFigure ::
  -- | text format of the caption
  Format ->
  FigureSpec ->
  PlotM Block
toFigure fmt spec = do
  target <- figurePath spec
  scp <- pack <$> sourceCodePath spec
  sourceLabel <- asksConfig sourceCodeLabel -- Allow the possibility for non-english labels
  let srcLink = link scp mempty (str sourceLabel)
      attrs' = blockAttrs spec
      withSource' = withSource spec
      captionText = fromList $ fromMaybe mempty (captionReader fmt $ caption spec)
      captionLinks = mconcat [" (", srcLink, ")"]
      caption' = if withSource' then captionText <> captionLinks else captionText
  builder attrs' target caption'
  where
    builder = case saveFormat spec of
      HTML -> interactiveBlock
      LaTeX -> latexInput
      _ -> figure

figure ::
  Attr ->
  FilePath ->
  Inlines ->
  PlotM Block
-- To render images as figures with captions, the target title
-- must be "fig:"
-- Janky? yes
figure as fp caption' =
  return . head . toList . para $
    imageWith as (pack fp) "fig:" caption'

-- TODO: also add the case where SVG plots can be
--       embedded in HTML output
-- embeddedSVGBlock ::
--   Attr ->
--   FilePath ->
--   Inlines ->
--   PlotM Block
-- embeddedSVGBlock _ fp caption' = do
--   svgsource <- liftIO $ T.readFile fp
--   renderedCaption <- writeHtml caption'
--   return $
--     RawBlock
--       "html5"
--       [st|
-- <figure>
--     <svg>
--     #{svgsource}
--     </svg>
--     <figcaption>#{renderedCaption}</figcaption>
-- </figure>
--     |]

latexInput :: Attr -> FilePath -> Inlines -> PlotM Block
latexInput _ fp caption' = do
  renderedCaption' <- writeLatex caption'
  let renderedCaption =
        if renderedCaption' /= ""
          then [st|\caption{#{renderedCaption'}}|]
          else ""
  return $
    RawBlock
      "latex"
      [st|
    \begin{figure}
        \centering
        \input{#{pack $ normalizePath $ fp}}
        #{renderedCaption}
    \end{figure}
        |]
  where
    normalizePath = map f
      where
        f '\\' = '/'
        f x = x

interactiveBlock ::
  Attr ->
  FilePath ->
  Inlines ->
  PlotM Block
interactiveBlock _ fp caption' = do
  -- TODO: should we instead include the scripts in the "include-after"
  --       template variable?
  --       See https://github.com/jgm/pandoc/issues/6582
  htmlpage <- liftIO $ T.readFile fp
  renderedCaption <- writeHtml caption'
  return $
    RawBlock
      "html5"
      [st|
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

writeLatex :: Inlines -> PlotM Text
writeLatex is = liftIO $ handleError $ runPure $ writeLaTeX def document
  where
    document = Pandoc mempty [Para . toList $ is]

-- | Extract the plot-relevant content from inside of a full HTML document.
-- Scripts contained in the <head> tag are extracted, as well as the entirety of the
-- <body> tag.
extractPlot :: Text -> Text
extractPlot t =
  let tags = canonicalizeTags $ parseTagsOptions parseOptionsFast t
      extracted = headScripts tags <> [inside "body" tags]
   in mconcat $ renderTags <$> (deferScripts <$> extracted)
  where
    headScripts = partitions (~== ("<script>" :: String)) . inside "head"

-- | Get content inside a tag, e.g. /inside "body"/ returns all tags
-- between /<body>/ and /</body>/
inside :: Text -> [Tag Text] -> [Tag Text]
inside t = init . tail . tgs
  where
    tgs = takeWhile (~/= TagClose t) . dropWhile (~/= TagOpen t [])

data ScriptTag
  = InlineScript [Attribute Text]
  | ExternalScript [Attribute Text]

fromTag :: Tag Text -> Maybe ScriptTag
fromTag (TagOpen "script" attrs) =
  Just $
    if "src" `elem` map fst attrs
      then ExternalScript attrs
      else InlineScript attrs
fromTag _ = Nothing

toTag :: ScriptTag -> Tag Text
toTag (InlineScript t) = TagOpen "script" t
toTag (ExternalScript t) = TagOpen "script" t

deferScript :: ScriptTag -> ScriptTag
deferScript (InlineScript attrs) = InlineScript $ nub $ attrs <> [("type", "module")]
deferScript (ExternalScript attrs) = ExternalScript $ nub $ attrs <> [("defer", mempty)]

-- | Replace /<script src=...>/ tags with /<script src=... defer>/,
-- and inline scripts as /<script type="module">/.
-- This makes scripts execute only after HTML parsing has finished.
deferScripts :: [Tag Text] -> [Tag Text]
deferScripts = fmap (\t -> maybe t (toTag . deferScript) (fromTag t))
