# dosearch 1.0.10

  * Added support for user-defined context-specific variables for LDAGs via `con_vars` in `control`.
  * Removed C++11 from system requirements (for CRAN).

# dosearch 1.0.9

  * Added several sanity checks for the input distributions and the graph.
  * Updated the documentation in various aspects. Importantly, it is now noted that `dosearch` is not complete for missing data problems.
  * A non-identifiable query is now reported as follows: "The query is non-identifiable by do-search" to further clarify that the search was not able to identify the query.
  * Added a new Vignette for the package.
  * Added a `plot` method via the `DiagrammeR` package to produce derivation plots more easily.
  * An error is now issued when the input graph is too large.
  * Fixed an example on transportability in the `dosearch` function documentation.

# dosearch 1.0.8

  * Added citation for the related article in the Journal of Statistical Software.

# dosearch 1.0.7

  * Changed the default option of the search heuristic to `FALSE` for all instances.
  * Implemented minor performance tweaks to the search algorithm.
  * Added more options for benchmarking.
  * Restored a legacy control parameter for internal use.
  * Moved `igraph` and `dagitty` packages to 'Suggests'.

# dosearch 1.0.6

  * Fixed a case where the search would continue unnecessarily for some time after discovering the target distribution.

# dosearch 1.0.5

  * The resulting list of the 'dosearch' function call now has the class `dosearch` with print and summary methods.
  * Added accessor functions `get_formula`, `get_benchmark` and `is_identifiable` to query an object of class `dosearch`.
  * The function `get_derivation` is now repurposed as an accessor function, and `dosearch` is now the only function for query identification.
  * The `graph` argument of `dosearch` now supports the `igraph` format used by the `causaleffect` package (see `dosearch` documentation for examples).
  * The `graph` argument of `dosearch` now supports DAGs created using the `dagitty` package (see `dosearch` documentation for examples).
  * Fixed an issue when using `benchmark = TRUE` in `control`.

# dosearch 1.0.4

  * Fixed compilation issues on some platforms.

# dosearch 1.0.3

  * Now supports context specific independence relations (CSIs) via labeled directed acyclic graphs (LDAGs). See the documentation and examples for more details.
  * The function `get_derivation` can now also be called via `dosearch`.
  * Bidirected edges can now also be given as `<->`.
  * Added a new control parameter, `time_limit`, which will terminate the search when the given amount of time (in hours) has passed (only when `benchmark = TRUE`).
  * Some redundant control options have been removed.
  * Fixed an issue with derivation rules when missing data was present.
  * Fixed an another minor issue regarding missing data.
  * Fixed a rare issue with formula generation.

# dosearch 1.0.2

  * Added an example on how to produce an image from the DOT derivation.
  * Added a warning when the response indicator for a proxy variable is not present in any data source.

# dosearch 1.0.1

  * Added a Vignette describing the search procedure.
