# dosearch: an R package for causal effect identification from multiple incomplete data sources

Identification of causal effects from arbitrary observational and experimental probability distributions via do-calculus and standard probability manipulations 
using a search-based algorithm. Allows for the presence of mechanisms related to selection bias (Bareinboim, E. and Tian, J. (2015) <http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>), 
transportability (Bareinboim, E. and Pearl, J. (2014) <http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf>), 
missing data (Mohan, K. and Pearl, J. and Tian., J. (2013) <http://ftp.cs.ucla.edu/pub/stat_ser/r410.pdf>) and 
arbitrary combinations of these. Also supports identification in the presence of context-specific independence (CSI) relations 
through labeled directed acyclic graphs (LDAG). 

For details on CSIs see Corander et al. (2019) <doi:10.1016/j.apal.2019.04.004>. 
For further information on the search-based approach see the paper [Causal Effect Identification from Multiple Incomplete Data Sources: A General Search-based Approach](https://arXiv:1902.01073).

## Installation
You can install the latest release version from CRAN:
```R
install.packages("dosearch")
```

Alternatively, you can install the latest development version by using the devtools package:
```R
install.packages("devtools")
devtools::install_github("santikka/dosearch")
```

## Recent changes (for all changes, see NEWS file).

### dosearch 1.0.6
 * Fixed a case where the search would continue unnecessarily for some time after discovering the target distribution.

### dosearch 1.0.5
 * The resulting list of the 'dosearch' function call now has the class 'dosearch' with print and summary methods.
 * Added accessor functions 'get_formula', 'get_benchmark' and 'is_identifiable' to query an object of class 'dosearch'.
 * The function 'get_derivation' is now repurposed as an accessor function, only 'dosearch' is now supported for query identification.
 * The 'graph' argument of 'dosearch' now supports the igraph format used by the 'causaleffect' package (see 'dosearch' examples).
 * The 'graph' argument of 'dosearch' now supports DAGs created using the 'dagitty' package (see 'dosearch' examples).
 * Fixed an issue when using control(benchmark = TRUE).

### dosearch 1.0.4
 * Fixed compilation issues on some platforms.

### dosearch 1.0.3
 * Now supports context specific independences (CSIs) via labeled directed acyclic graphs (LDAGs). See the documentation and examples for more details.
 * The function 'get_derivation' can now also be called via 'dosearch'.
 * Bidirected edges can now also be given as '<->'.
 * Added a new control option, 'time_limit', which will terminate the search when the given amount of time (in hours) has passed (only when benchmark = TRUE).
 * Some redundant control options have been removed.
 * Fixed an issue with derivation rules when missing data was present.
 * Fixed an another minor issue regarding missing data.
 * Fixed a rare issue with formula generation.

