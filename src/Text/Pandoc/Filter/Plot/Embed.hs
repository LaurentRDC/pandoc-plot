{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : $header$
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
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
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Text.HTML.TagSoup
  ( Tag (TagClose, TagOpen),
    canonicalizeTags,
    parseOptionsFast,
    parseTagsOptions,
    partitions,
    renderTags,
    (~/=),
    (~==),
  )
import Text.Pandoc.Builder as Builder
  ( Inlines,
    figureWith,
    fromList,
    imageWith,
    link,
    plain,
    simpleCaption,
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
      captionText = fromList $ fromMaybe mempty (captionReader fmt $ caption spec)
      captionLinks = mconcat [" (", srcLink, ")"]
      caption' = if withSource spec then captionText <> captionLinks else captionText
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
figure as fp caption' =
  return . head . toList $
    -- We want the attributes both on the Figure element and the contained Image element
    -- so that pandoc-plot plays nice with pandoc-crossref and other filters
    figureWith as (simpleCaption (plain caption')) $
      plain $
        imageWith as (pack fp) mempty caption'

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
latexInput (_, _, attrs) fp caption' = do
  renderedCaption' <- writeLatex caption'
  let renderedCaption =
        if renderedCaption' /= ""
          then [st|\caption{#{renderedCaption'}}|]
          else ""
  -- We need to propagate extra attributes, for example, to control
  -- figure sizes. See #38
  let renderedExtraAttributes = mconcat $ [key <> "=" <> value | (key, value) <- attrs]
  return $
    RawBlock
      "latex"
      [st|
    \begin{figure}
        \centering
        \includegraphics[#{renderedExtraAttributes}]{#{pack $ normalizePath $ fp}}
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
   in -- In the past (e.g. commit 8417b011ccb20263427822c7447840ab4a30a41e), we used to
      -- make all JS scripts 'deferred'. This turned out to be problematic for plotly
      -- specifically (see issue #39). In the future, we may want to defer scripts for
      -- certain toolkits, but that's a testing nightmare...
      mconcat $ renderTags <$> extracted
  where
    headScripts = partitions (~== ("<script>" :: String)) . inside "head"

-- | Get content inside a tag, e.g. /inside "body"/ returns all tags
-- between /<body>/ and /</body>/
inside :: Text -> [Tag Text] -> [Tag Text]
inside t = init . tail . tgs
  where
    tgs = takeWhile (~/= TagClose t) . dropWhile (~/= TagOpen t [])
