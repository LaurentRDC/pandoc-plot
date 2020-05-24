
import Criterion.Main (bench, defaultMain, nf)
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Plot
import Text.Pandoc.Filter.Plot.Internal

main :: IO ()
main = 
    defaultMain []