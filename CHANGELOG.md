# Change log

pandoc-plot uses [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

Release 0.5.0.0
---------------

* `pandoc-plot` will now process documents in parallel. This should dramatically speed up processing of large documents with lots of figures. This functionality is also exposed in libraries via `plotTransformP`.

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