# Change log

pandoc-plot uses [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

Release 1.2.1
-------------

* Added the ability to save plots as LaTeX directly from the GNUplot toolkit. To do this, simply set the output format to `latex`. The figure content will be embedded in the output document, which only makes sense for final conversion to LaTeX. For example:

  ````markdown
    ```{.gnuplot format=latex caption="This is a test."}
    ...
    ```
  ````
  This patch was contributed by Saku Laesvuori.

Release 1.2.0
-------------

* Fixed an issue where code blocks nested in other structures were detected properly. For example, in the following LaTeX snippet, plots would not be detected properly:
```latex
\begin{column}
  \begin{minted}[]{matplotlib}
  ...
  \end{minted}
\end{column}
```
Nested figures are not correctly identified.

* The executables are now built with Pandoc 2.13. Pandoc 2.11 and Pandoc 2.12 are still supported.

Release 1.1.1
-------------

* Error messages now include the source script and line numbers.
* The executables are now built with Pandoc 2.12. Pandoc 2.11 is still supported.

Release 1.1.0
-------------

* Added the [PlantUML](https://plantuml.com) toolkit (#18). Diagrams can be generated like so:

  ````markdown

  ```{.plantuml}
  @startuml
  Bob->Alice : hello
  @enduml
  ```

  ````
* Changed versioning scheme to match more common Major.Minor.Bugfix.

Release 1.0.2.1
---------------

* `pandoc-plot` will now only render at most `N` figures in parallel, where `N` is the number of available CPU cores.
* Fixed an issue where error message would get mangled in strict-mode.

Release 1.0.2.0
---------------

* Added the ability to run `pandoc-plot` in **strict mode**. By default, `pandoc-plot` leaves code blocks unchanged if a figure fails to be rendered.  In strict mode, `pandoc-plot` will immediately halt if it encounters a problem, such as a missing toolkit. You can activate strict mode via configuration:

```yaml
strict: true
```

* Added the ability to set command-line arguments for interpreters via configuration. For example, if you want to run the Matplotlib toolkit with all warnings shown:

```yaml
# Possible parameters for the Matplotlib toolkit
matplotlib:
  executable: python
  command_line_arguments: -Wa
```

Or if you want `julia` to use more than one thread:

```yaml
# Possible parameters for the Plotsjl toolkit
plotsjl:
  executable: julia
  command_line_arguments: --threads auto --optimize=0
```

* Fixed an issue where invoking the `plotsjl` toolkit on Windows would sometimes fail with the error: Unknown system error 50.
* Fixed an issue with R-based toolkits on Windows not being detected properly.


Release 1.0.1.0
---------------

* Added the ability to change the "Source code" label to other languages via configuration.
* Added syntax highlighting to the linked source code.
* Fixed an issue where code blocks with unicode symbols (e.g. greek letters) would trip up pandoc-plot (#16).

Release 1.0.0.0
---------------

* Added support for Pandoc 2.11. Unfortunately, there is no way to support prior versions of Pandoc at the same time.
* With release 1.0.0.0, `pandoc-plot` has stabilized. The Haskell library API will not change until version 2+. 

Release 0.9.4.0
---------------

* Fixed an issue where the current working directory was changed. This prevented users from referring to files in scripts with relative paths (#2).

Release 0.9.3.0
---------------

* Added executable caching: repeated usage of a particular toolkit will be faster because executables are only looked-for once.
* Reverting the change from 0.8.1.0: internal machinery of `pandoc-plot` has been moved to the `Text.Pandoc.Filter.Plot.Internal` module, where there is no guarantee of backwards-compatibility after 1.0.0.
* Removed the `makePlot` function, which could not take advantage of multithreading and other key features of `pandoc-plot`.
* Fixed an issue where files required for tests were missing from source tarballs (#13).

Release 0.9.2.0
---------------

* Fixed an issue where executables located on paths with spaces would not be invoked correctly (#12).
* Fixed an issue where R-paths were not normalized correctly.
* Fixed an issue where executables specified in configuration that did not exist would crash `pandoc-plot`.
* Fixed an issue where some R-based toolkits appeared to be available, but were not.

Release 0.9.1.0
---------------

* Added the `file` parameter, which allows the user to read figure content from a file instead of using the code block content in documents. This is especially useful for complex figures, where you might want to have the help of your tooling in an IDE, for instance. Here's an example:

````markdown

```{.matplotlib file=myplot.py}
```

````
* Better error messages when specifying logger verbosity.
* Cleaning output directories with `pandoc-plot clean` now follows configuration values for logging. 
* Fixed an issue where configuration in metadata did not get parsed properly.

Release 0.9.0.0
---------------

* The `bokeh` toolkit now supports exporting plots as SVGs (#8).
* Interactive plots relying on javascript scripts will now defer loading the scripts (#9). 
* Added the `dependencies` argument, which tells `pandoc-plot` what files are important to a particular figure (#10). If a file listed in `dependencies` changes (for example, a data file), `pandoc-plot` will re-render the associated figure.
* Better heuristic to determine what `bokeh` plot to save. This allows the user to export plots like the [`bokeh.layouts` module](https://docs.bokeh.org/en/latest/docs/user_guide/layout.html#creating-layouts).
* Added support for the `dpi` parameter in `graphviz` and `mathematica`.
* Added support for MATLAB's new `exportgraphics` function introduced in MATLAB 2020a. Older versions fallback to using `saveas`.

Release 0.8.1.0
---------------

* The module `Text.Pandoc.Filter.Plot.Internal` is no longer exposed; instead, everything relevant is exposed by the `Text.Pandoc.Filter.Plot` module.
* Fixed an issue where script errors would be logged as debug messages.
* Interactive plots are now embedded directly in output (#7).

Release 0.8.0.0
---------------

* Added a new output format, HTML, to produce interactive plots. Not all renderers support it. You can try with Plotly/Python and Plotly/R as follows:

````markdown

```{.plotly_python format=html}
import plotly.express as px
df = px.data.election()
fig = px.scatter_ternary(df, a="Joly", b="Coderre", c="Bergeron")
```

````

* Added a new toolkit, [`bokeh`](https://bokeh.org/). This toolkit can take advantage of the new HTML interactive output.
* Added a new toolkit, [`plotsjl`](http://docs.juliaplots.org/latest/).
* Separated the detailed information from `README.md` and into a proper `MANUAL.md`. This is now the information which will be shown with `pandoc-plot --manual`.
* Exposed the `pandoc-plot` version via `Text.Pandoc.Filter.Plot.pandocPlotVersion`.

Release 0.7.2.1
---------------

* Fixed an issue where the `pandoc` version was not parsed properly, giving rise to errors when running `pandoc-plot`.
* Fixed an issue where logging errors were not always displayed.

Release 0.7.2.0
---------------

* Removed dependency on `open-browser` package.
* Starting with this version, `pandoc` 2.8 and 2.9 are **no longer supported** due to a breaking API change in `pandoc` 2.10.
* Executables are now built with GHC 8.10.1.

Release 0.7.1.0
---------------

* Better multi-threaded logging. Only one thread (the logging thread) performs IO on the log file. This prevents hang-ups when working on large documents. 

Release 0.7.0.0
---------------

* Added documentation on using `pandoc-plot` with LaTeX documents as well.
* Added preliminary support for logging to `pandoc-plot`. You can turn on this feature in the configuration as follows:

````yaml
logging:
    # Possible verbosity values: debug, error, warning, info, silent
    # debug level shows all messages
    # error level shows all but debug messages, etc.
    verbosity: info
    
    # OPTIONAL: log to file
    # Remove line below to log to stderr
    filepath: log.txt
````

* Removed dependencies `turtle`, `temporary`, `deepseq`, and `data-default-class`, resulting in improved build times by ~10%, and makes the executable smaller by 15-20%!

Release 0.6.1.0
---------------

* Made the functions `availableToolkits` and `unavailableToolkits` public.
* Minor documentation fixes.
* Executables are now built with GHC 8.8.3.

Release 0.6.0.0
---------------

New toolkits:
* Added support for the Plotly/R plotting library.
* Added support for Graphviz.

Other changes:
* The determination of which figures to re-render or not has been improved. For example, changing the caption will not trigger a re-render of a figure anymore.
* `pandoc-plot` will look for executables more thoroughly.
* `pandoc-plot toolkits` will now show the exact executable that is being used, if possible.
* Added a check when running the filter that the Pandoc version is at least 2.8. This is easier to understand that the default Pandoc warning on API incompatibility.
* Added the ability to write the example configuration to an arbitrary file using `pandoc-plot write-example-config`.
* Added the possibility to specify the configuration file via metadata. For example, in Markdown:

    ```markdown
    ---
    title: My document
    author: John Doe
    plot-configuration: /path/to/file.yml
    ---     
    ```

or on the command line:

```bash
pandoc --filter pandoc-plot -M plot-configuration=/path/to/file.yml ...
```
* Added the ability to specify configuration file to the `pandoc-plot clean` and `pandoc-plot toolkits` commands.

Release 0.5.0.0
---------------

* The `pandoc-plot` executable will now process documents in parallel. This should dramatically speed up processing of large documents with lots of figures. 
This happens automatically through the function `plotTransform`.
* Added a benchmarking suite.
* Added `defaultConfiguration` so that people don't have to install the `data-default` package to get access to default configuration values. 
* Added a check for the `matplotlib` toolkit, preventing users from using `matplotlib.pyplot.show` in figures. This would halt `pandoc-plot`.

Release 0.4.0.1
---------------

* Fixed an issue where the `pandoc-plot` executable could not be built outside of its git repository.

Release 0.4.0.0
---------------

* Updated documentation.
* Added a `--full-version` flag to the executable, which includes which version of pandoc/pandoc-types was used, as well as the git revision.
* Added the `clean` command to the executable. This can be used to clean-up output files produced by pandoc-plot.
* Changed the flag `--write-example-config` to the command `write-example-config`.
* Added the top-level function `cleanOutputDir` to clean output of pandoc-plot. This is only accessible if `pandoc-plot` is used as a library.
* Added a distinction between failure to render a figure because of a mistake, and failing to render a figure because the toolkit is not installed. `pandoc-plot` will give better error messages in the latter case.

Release 0.3.0.0
---------------

* Added more examples.
* Added MacOS binaries built via Azure pipelines.
* BREAKING CHANGE: Parsing captions based on source file was not working. Captions format can be specified in the configuration file. This unfortunately changes the type signature of a few high-level functions.

Release 0.2.2.0
---------------

* Fixed an issue where paths with spaces would not work (issue #2).
* Added Linux binaries built via Azure pipelines.

Release 0.2.1.0
---------------

* Improved documentation.

Release 0.2.0.0
---------------

* Added support for gnuplot.
* Added more tests for all toolkits.

* Fixed an issue where the package could not be installed because a source file was not included in the cabal file.

Release 0.1.0.0
---------------

* Initial release