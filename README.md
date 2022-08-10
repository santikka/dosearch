# dosearch: an R package for causal effect identification from multiple incomplete data sources

<!-- badges: start -->
[![R-CMD-check](https://github.com/santikka/dosearch/workflows/R-CMD-check/badge.svg)](https://github.com/santikka/dosearch/actions)
[![Codecov test coverage](https://codecov.io/gh/santikka/dosearch/branch/master/graph/badge.svg)](https://app.codecov.io/gh/santikka/dosearch?branch=master)
<!-- badges: end -->

Identification of causal effects from arbitrary observational and experimental 
probability distributions via do-calculus and standard probability manipulations 
using a search-based algorithm. Allows for the presence of mechanisms related
to selection bias (Bareinboim, E. and Tian, J. (2015) 
<http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>), 
transportability (Bareinboim, E. and Pearl, J. (2014) 
<http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf>), 
missing data (Mohan, K. and Pearl, J. and Tian., J. (2013) 
<http://ftp.cs.ucla.edu/pub/stat_ser/r410.pdf>) and 
arbitrary combinations of these. Also supports identification in the presence 
of context-specific independence (CSI) relations 
through labeled directed acyclic graphs (LDAG). For details on CSIs see 
Corander et al. (2019) <doi:10.1016/j.apal.2019.04.004>. 
For further information on the search-based approach see the paper 
[Causal Effect Identification from Multiple Incomplete Data Sources: 
A General Search-based Approach](https://arxiv.org/abs/1902.01073).

## Installation
You can install the latest release version from CRAN:
```R
install.packages("dosearch")
```

Alternatively, you can install the latest development version:
```R
install.packages("devtools")
remotes::install_github("santikka/dosearch")
```

## Recent changes (for all changes, see NEWS file).

### dosearch 1.0.7
 * Changed the default option of the search heuristic to FALSE for all instances.
 * Implemented minor performance tweaks to the search.
 * Added more options for benchmarking.
 * Restored a legacy control parameter for internal use.
 * Moved 'igraph' and 'dagitty' packages to 'Suggests'.

### dosearch 1.0.6
 * Fixed a case where the search would continue unnecessarily for some time after discovering the target distribution.

### dosearch 1.0.5
 * The resulting list of the 'dosearch' function call now has the class 'dosearch' with print and summary methods.
 * Added accessor functions 'get_formula', 'get_benchmark' and 'is_identifiable' to query an object of class 'dosearch'.
 * The function 'get_derivation' is now repurposed as an accessor function, only 'dosearch' is now supported for query identification.
 * The 'graph' argument of 'dosearch' now supports the igraph format used by the 'causaleffect' package (see 'dosearch' examples).
 * The 'graph' argument of 'dosearch' now supports DAGs created using the 'dagitty' package (see 'dosearch' examples).
 * Fixed an issue when using control(benchmark = TRUE).

