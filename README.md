# pandoc-plot 

## A Pandoc filter to generate figures from code blocks in documents

[![Hackage version](https://img.shields.io/hackage/v/pandoc-plot.svg)](http://hackage.haskell.org/package/pandoc-plot) [![Stackage version (latest long-term service)](http://stackage.org/package/pandoc-plot/badge/lts)](http://stackage.org/lts/package/pandoc-plot) [![Stackage version (nightly)](http://stackage.org/package/pandoc-plot/badge/nightly)](http://stackage.org/nightly/package/pandoc-plot) [![Conda Version](https://img.shields.io/conda/vn/conda-forge/pandoc-plot.svg)](https://anaconda.org/conda-forge/pandoc-plot) [![license](https://img.shields.io/badge/license-GPLv2+-lightgray.svg)](https://www.gnu.org/licenses/gpl.html) 

`pandoc-plot` turns code blocks present in your documents (Markdown, LaTeX, etc.) into embedded figures, using your plotting toolkit of choice, including Matplotlib, ggplot2, MATLAB, Mathematica, and more.

## Overview

This program is a [Pandoc](https://pandoc.org/) filter. It can therefore
be used in the middle of conversion from input format to output format,
replacing code blocks with figures.

The filter recognizes code blocks with classes that match plotting
toolkits. For example, using the `matplotlib` toolkit:

```` markdown
# My document

This is a paragraph.

```{.matplotlib}
import matplotlib.pyplot as plt

plt.figure()
plt.plot([0,1,2,3,4], [1,2,3,4,5])
plt.title('This is an example figure')
```
````

Putting the above in `input.md`, we can then generate the plot and embed
it in an HTML page:

``` bash
pandoc --filter pandoc-plot input.md --output output.html
```

*Note that pandoc-plot only works with pandoc \>= 2.10 because of some
breaking changes in pandoc’s API.*

## Supported toolkits

`pandoc-plot` currently supports the following plotting toolkits
(**installed separately**):

  - `matplotlib`: plots using the [matplotlib](https://matplotlib.org/)
    Python library;
  - `plotly_python` : plots using the
    [plotly](https://plotly.com/python/) Python library;
  - `plotly_r`: plots using the [plotly](https://plotly.com/r/) R
    library
  - `matlabplot`: plots using [MATLAB](https://www.mathworks.com/);
  - `mathplot` : plots using
    [Mathematica](https://www.wolfram.com/mathematica/);
  - `octaveplot`: plots using [GNU
    Octave](https://www.gnu.org/software/octave/);
  - `ggplot2`: plots using [ggplot2](https://ggplot2.tidyverse.org/);
  - `gnuplot`: plots using [gnuplot](http://www.gnuplot.info/);
  - `graphviz`: graphs using [Graphviz](http://graphviz.org/);
  - `bokeh`: plots using the [Bokeh](https://bokeh.org/) visualization library;

To know which toolkits are useable on *your machine* (and which ones are
not available), you can check with the `toolkits` command:

``` bash
pandoc-plot toolkits
```

The `toolkits` command is described in its own section below.

**Wish your plotting toolkit of choice was available? Please [raise an
issue](https://github.com/LaurentRDC/pandoc-plot/issues)\!**

## Documentation

You can find more information in the documentation, available either in the
source repository file `MANUAL.md`, or via the command `pandoc-plot --manual`.

## Installation

### Binaries and Installers

Windows, Linux, and Mac OS binaries are available on the [GitHub release
page](https://github.com/LaurentRDC/pandoc-plot/releases). There are
also Windows installers.

### conda

Like `pandoc`, `pandoc-plot` is available as a package installable with
[`conda`](https://docs.conda.io/en/latest/). [Click here to see the
package page](https://anaconda.org/conda-forge/pandoc-plot).

To install in the current environment:

``` sh
conda install -c conda-forge pandoc-plot
```

### winget

You can install `pandoc-plot` from the [Windows Package
Manager](https://github.com/microsoft/winget-cli) `winget` (just like
`pandoc`). To install:

``` sh
winget install pandoc-plot
```

### From Hackage/Stackage

`pandoc-plot` is available on
[Hackage](http://hackage.haskell.org/package/pandoc-plot) and
[Stackage](https://www.stackage.org/nightly/package/pandoc-plot). Using
the [`cabal-install`](https://www.haskell.org/cabal/) tool:

``` bash
cabal update
cabal install pandoc-plot
```

or

``` bash
stack update
stack install pandoc-plot
```

### From source

[![Build
Status](https://dev.azure.com/laurentdecotret/pandoc-plot/_apis/build/status/LaurentRDC.pandoc-plot?branchName=master)](https://dev.azure.com/laurentdecotret/pandoc-plot/_build/latest?definitionId=5&branchName=master)

Building from source can be done using
[`stack`](https://docs.haskellstack.org/en/stable/README/) or
[`cabal`](https://www.haskell.org/cabal/):

``` bash
git clone https://github.com/LaurentRDC/pandoc-plot
cd pandoc-plot
stack install # Alternatively, `cabal install`
```
