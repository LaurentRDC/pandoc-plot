# Preamble example

This folder contains a showcase the use of `pandoc-plot` with preamble scripts. You will need Python 3+, and the `matplotlib` package.

Two things are of note: the file `include.py` will be included in a figure using the `preamble` parameter. The path can be absolute, or relative to the current working directory.

To see the source code used to generate figures, we activate source code links with `source=true` or `source=True`.

To compile this file, you must be in the `preamble` directory. Then:

```bash
pandoc --standalone --filter pandoc-plot -i preamble.md -o example.html
```