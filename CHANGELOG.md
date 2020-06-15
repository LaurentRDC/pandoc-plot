# Change log

pandoc-plot uses [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

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

* Removed dependencies `turtle`, `temporary`, `deepseq`, and `data-default-class`, resulting in improved build times by ~10%!

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