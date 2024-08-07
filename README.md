
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dosearch

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/santikka/dosearch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/santikka/dosearch/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/santikka/dosearch/branch/master/graph/badge.svg)](https://app.codecov.io/gh/santikka/dosearch?branch=master)
[![CRAN
version](https://www.r-pkg.org/badges/version/dosearch)](https://CRAN.R-project.org/package=dosearch)

<!-- badges: end -->

The `dosearch` [R](https://www.r-project.org/) package facilitates
identification of causal effects from arbitrary observational and
experimental probability distributions via do-calculus and standard
probability manipulations using a search-based algorithm (Tikka et al.,
2019, 2021). Formulas of identifiable target distributions are returned
in character format using LaTeX syntax. The causal graph may
additionally include mechanisms related to:

- Selection bias (Bareinboim and Tian, 2015)
- Transportability (Bareinboim and Pearl, 2014)
- Missing data (Mohan et al., 2013)
- Context-specific independence (Corander et al., 2019)

See the package vignette or the references for further information.

### Citing the package

If you use the `dosearch` package in a publication, please cite the
corresponding paper in the Journal of Statistical Software:

Tikka S, Hyttinen A, Karvanen J (2021). “Causal Effect Identification
from Multiple Incomplete Data Sources: A General Search-Based Approach.”
*Journal of Statistical Software*, 99(5), 1–40.
[doi:10.18637/jss.v099.i05](https://doi.org/10.18637/jss.v099.i05).

## Installation

You can install the latest release version from CRAN:

``` r
install.packages("dosearch")
```

Alternatively, you can install the latest development version of
`dosearch`:

``` r
# install.packages("devtools")
devtools::install_github("santikka/dosearch")
```

## Examples

``` r
# back-door formula
data <- "p(x,y,z)"
query <- "p(y|do(x))"
graph <- "
  x -> y
  z -> x
  z -> y
"
dosearch(data, query, graph)
#> \sum_{z}\left(p(z)p(y|x,z)\right)

# front-door formula
graph <- "
  x -> z
  z -> y
  x <-> y
"
dosearch(data, query, graph)
#> \sum_{z}\left(p(z|x)\sum_{x}\left(p(x)p(y|z,x)\right)\right)

# the 'napkin' graph
data <- "p(x,y,z,w)"
graph <- "
  x -> y
  z -> x
  w -> z
  x <-> w
  w <-> y
"
dosearch(data, query, graph)
#> \frac{\sum_{w}\left(p(w)p(y,x|z,w)\right)}{\sum_{y}\sum_{w}\left(p(w)p(y,x|z,w)\right)}

# case-control design
data <- "
  p(x*,y*,r_x,r_y)
  p(y)
"
graph <- "
  x -> y
  y -> r_y
  r_y -> r_x
"
md <- "r_x : x, r_y : y"
dosearch(data, query, graph, missing_data = md)
#> \frac{\left(p(y)p(x|r_x = 1,y,r_y = 1)\right)}{\sum_{y}\left(p(y)p(x|r_x = 1,y,r_y = 1)\right)}
```

## References

- Tikka S, Hyttinen A, Karvanen J (2021). “Causal effect identification
  from multiple incomplete data sources: a general search-based
  approach.” *Journal of Statistical Software*, 99(5), 1–40.
  [doi:10.18637/jss.v099.i05](https://doi.org/10.18637/jss.v099.i05)
- Tikka S, Hyttinen A, Karvanen J (2019). “Identifying causal effects
  via context-specific independence relations.” In *Proceedings of the
  33rd Annual Conference on Neural Information Processing Systems*.
  (<https://papers.nips.cc/paper/2019/hash/d88518acbcc3d08d1f18da62f9bb26ec-Abstract.html>)
- Bareinboim E, Tian J (2015). “Recovering causal effects from selection
  bias.” In *Proceedings of the Twenty-Ninth AAAI Conference on
  Artificial Intelligence*,
  (<http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>)
- Bareinboim E, Pearl J (2014). “Transportability from multiple
  environments with limited Experiments: completeness Results.” In
  *Advances of Neural Information Processing Systems 27*,
  (<http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf>)
- Mohan K, Pearl J, Tian J (2013). “Graphical models for inference with
  missing data.” In *Advances of Neural Information Processing Systems
  26*, (<http://ftp.cs.ucla.edu/pub/stat_ser/r410.pdf>)
- Corander J, Hyttinen A, Kontinen J, Pensar J, Väänänen J (2019). “A
  logical approach to context-specific independence.” *Annals of Pure
  and Applied Logic*, 170(9), 975–992.
  [doi:10.1016/j.apal.2019.04.004](https://doi.org/10.1016/j.apal.2019.04.004)
