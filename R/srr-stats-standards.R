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
#' @srrstatsNA {G2.4d, G2.4e, G2.5} Factor input is not supported.
#' @srrstatsNA {G2.7, G2.10, G2.11, G2.12} Tabular forms are not supported.
#' @srrstatsNA {G2.9} There is no loss of information.
#' @srrstatsNA {G2.13, G2.14, G2.14a, G2.14b, G2.14c, G2.15, G2.16, G5.3}
#'   The package does not deal with data in the statistical sense, thus no NA
#'   values or non-finite values are considered. The "data" manipulated by the
#'   package is purely symbolic.
#' @srrstatsNA {G3.1, G3.1a} No covariance calculations are performed.
#' @srrstatsNA {G4.0} The package does not write to local files.
#' @srrstatsNA {G5.4c} Stored values are not drawn
NULL
