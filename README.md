
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dosearch

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/santikka/dosearch/workflows/R-CMD-check/badge.svg)](https://github.com/santikka/dosearch/actions)
[![Codecov test
coverage](https://codecov.io/gh/santikka/dosearch/branch/master/graph/badge.svg)](https://app.codecov.io/gh/santikka/dosearch?branch=master)

The `dosearch` [R](https://www.r-project.org/) package factilitates
identification of causal effects from arbitrary observational and
experimental probability distributions via do-calculus and standard
probability manipulations using a search-based algorithm (Tikka et al.,
2019, 2021). Formulas of identifiable target distributions are returned
in character format using LaTeX syntax. The causal graph may
additionally include mechanisms related to:

-   Selection bias (Bareinboim and Tian, 2015)
-   Transportability (Bareinboim and Pearl, 2014)
-   Missing data (Mohan et al., 2013)
-   Context-specific independence (Corander et al., 2019)

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
# backdoor formula
data <- "p(x,y,z)"
query <- "p(y|do(x))"
graph <- "
  x -> y
  z -> x
  z -> y
"
dosearch(data, query, graph)
#> \sum_{z}\left(p(z)p(y|x,z)\right)

# frontdoor formula
graph <- "
  x -> z
  z -> y
  x <-> y
"
dosearch(data, query, graph)
#> \sum_{z}\left(p(z|x)\sum_{x}\left(p(x)p(y|z,x)\right)\right)

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

-   S. Tikka, A. Hyttinen, J. Karvanen, **Causal effect identification
    from multiple incomplete data sources: a general search-based
    approach**, *Journal of Statistical Software*, 99(5), 1–40, 2021.
    ([published
    version](https://www.jstatsoft.org/index.php/jss/article/view/v099i05),
    [arXiv](https://arxiv.org/abs/1902.01073))
-   S. Tikka, A. Hyttinen, J. Karvanen, **Identifying causal effects via
    context-specific independence relations**, In *Proceedings of the
    33rd Annual Conference on Neural Information Processing Systems*,
    2019. ([published
          version](https://papers.nips.cc/paper/8547-identifying-causal-effects-via-context-specific-independence-relations),
          [arXiv](https://arxiv.org/abs/2009.09768))
-   E. Bareinboim and J. Tian, **Recovering causal effects from
    selection bias**, In *Proceedings of the Twenty-Ninth AAAI
    Conference on Artificial Intelligence*,
    2015. (<http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>)
-   E. Bareinboim and J. Pearl, **Transportability from multiple
    environments with limited Experiments: completeness Results**, In
    *Advances of Neural Information Processing Systems 27*,
    2014. (<http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf>)
-   K. Mohan, J. Pearl, and J. Tian, **Graphical Models for Inference
    with Missing Data**, In *Advances of Neural Information Processing
    Systems 26*,
    2013. (<http://ftp.cs.ucla.edu/pub/stat_ser/r410.pdf>)
-   J. Corander, A. Hyttinen, J. Kontinen, J. Pensar, and J. Väänänen,
    **A logical approach to context-specific independence**, *Annals of
    Pure and Applied Logic*, 170(9), 975–992, 2019.
    (<https://doi.org/10.1016/j.apal.2019.04.004>)
