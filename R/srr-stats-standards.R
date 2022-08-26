#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#' @srrstatsVerbose TRUE
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
#' @srrstatsNA {G1.6, G5.4b}
#'   There are no alternative implementations for comparison.
#' @srrstatsNA {G2.3a} `match.arg` is never used or needed.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} Factor input is not supported.
#' @srrstatsNA {G2.7, G2.10, G2.11, G2.12} Tabular forms are not supported.
#' @srrstatsNA {G2.9} There is no loss of information.
#' @srrstatsNA {G2.13, G2.14, G2.14a, G2.14b, G2.14c, G2.15, G2.16, G5.3}
#'   The package does not deal with data in the statistical sense, thus no NA
#'   values or non-finite values are considered. The "data" manipulated by the
#'   package is purely symbolic.
#' @srrstatsNA {G3.1, G3.1a} No covariance calculations are performed.
#' @srrstatsNA {G4.0} The package does not write to local files.
#' @srrstatsNA {G5.4c} Stored values are not drawn.
#' @srrstatsNA {G5.6b} There is nothing stochastic to set seeds for in the
#'   tests.
#' @srrstatsNA {G5.9, G5.9a, G5.9b} Data is in symbolic form without noise.
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} There are no extended tests.
#' @srrstatsNA {NW2.8} Matrix inputs are not supported.
#' @srrstatsNA {NW3.0} Sparse matrices are not used.
#' @srrstatsNA {NW3.1} Adjacency matrices are not used in R code.
#' @srrstatsNA {NW4.1a} No claims of improvement are made.
#' @srrstatsNA {NW4.4a} There is no default maximum number of iterations or
#'   a default time limit.
#' @srrstatsNA {NW4.6, NW4.7} No grouping or dimensionality reduction is
#'   carried out.
#' @srrstatsNA {NW5.6} No grouping is carried out.
#' @srrstatsNA {NW6.1, NW6.2} Data is not labeled.
NULL
