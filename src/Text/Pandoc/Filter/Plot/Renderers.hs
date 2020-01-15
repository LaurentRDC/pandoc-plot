{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2020
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

Specification of renderers.
-}

module Text.Pandoc.Filter.Plot.Renderers (
      module Text.Pandoc.Filter.Plot.Renderers.Matplotlib
    , module Text.Pandoc.Filter.Plot.Renderers.Plotly
    , module Text.Pandoc.Filter.Plot.Renderers.Matlab
    , module Text.Pandoc.Filter.Plot.Renderers.Mathematica
) where

import Text.Pandoc.Filter.Plot.Renderers.Matplotlib
import Text.Pandoc.Filter.Plot.Renderers.Plotly
import Text.Pandoc.Filter.Plot.Renderers.Matlab
import Text.Pandoc.Filter.Plot.Renderers.Mathematica
