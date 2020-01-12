{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative                ((<|>))
import           Control.Monad                      (join)

import           Data.Default.Class                 (def)
import           Data.List                          (intersperse)
import           Data.Monoid                        ((<>))
import qualified Data.Text                          as T

import           Options.Applicative
import qualified Options.Applicative.Help.Pretty    as P

import           System.Directory                   (doesFileExist)
import           System.IO.Temp                     (writeSystemTempFile)

import           Text.Pandoc.Filter.Plot            (plotTransform)
import           Text.Pandoc.JSON                   (toJSONFilter)

import           Web.Browser                        (openBrowser)

import qualified Data.Version                       as V
import           Paths_pandoc_plot                  (version)

import           ManPage                            (embedManualHtml)

main :: IO ()
main = join $ execParser opts
    where
        opts = info (run <**> helper)
            (fullDesc
            <> progDesc "This pandoc filter generates plots from code blocks using a multitude of possible renderers. This allows to keep documentation and figures in perfect synchronicity."
            <> header "pandoc-plot - generate Matplotlib figures directly in documents."
            <> footerDoc (Just footer')
            )


toJSONFilterWithConfig :: IO ()
toJSONFilterWithConfig = do
    configExists <- doesFileExist ".pandoc-plot.yml"
    let configpath = if configExists
        then Just ".pandoc-plot.yml"
        else Nothing
    toJSONFilter (plotTransform def)


data Flag = Version
          | Manual
    deriving (Eq)


run :: Parser (IO ())
run = do
    versionP <- flag Nothing (Just Version) (long "version" <> short 'v'
                    <> help "Show version number and exit.")

    manualP  <- flag Nothing (Just Manual)  (long "manual"  <> short 'm'
                    <> help "Open the manual page in the default web browser and exit.")

    input    <- optional $ strArgument (metavar "AST")
    return $ go (versionP <|> manualP) input
    where
        go :: Maybe Flag -> Maybe String -> IO ()
        go (Just Version) _ = putStrLn (V.showVersion version)
        go (Just Manual)  _ = writeSystemTempFile "pandoc-plot-manual.html" (T.unpack manualHtml)
                                >>= \fp -> openBrowser ("file:///" <> fp)
                                >> return ()
        go Nothing _ = toJSONFilterWithConfig

manualHtml :: T.Text
manualHtml = T.pack $(embedManualHtml)


-- | Use Doc type directly because of newline formatting
footer' :: P.Doc
footer' = mconcat [
        P.text "Example usage with pandoc:"
    , P.line, P.line
    , P.indent 4 $ P.string "> pandoc --filter pandoc-plot input.md --output output.html"
    , P.line, P.line
    , P.text "If you use pandoc-plot in combination with other filters, you probably want to run pandoc-plot first. Here is an example with pandoc-crossref:"
    , P.line, P.line
    , P.indent 4 $ P.string "> pandoc --filter pandoc-plot --filter pandoc-crossref -i input.md -o output.pdf"
    , P.line, P.line
    , P.text "More information can be found via the manual (pandoc-plot --manual) or the repository README, located at"
    , P.line
    , P.indent 4 $ P.text "https://github.com/LaurentRDC/pandoc-plot"
    , P.line
    ]
