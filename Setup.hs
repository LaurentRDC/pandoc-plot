-- This script is used to build and install your package. Typically you don't
-- need to change it. The Cabal documentation has more information about this
-- file: <https://www.haskell.org/cabal/users-guide/installing-packages.html>.
import qualified Distribution.Simple

main :: IO ()
main = Distribution.Simple.defaultMain
