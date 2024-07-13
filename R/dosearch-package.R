#' Causal Effect Identification from Multiple Incomplete Data Sources
#'
#' @description Solves causal effect identifiability problems from arbitrary
#' observational and experimental distributions using a heuristic search.
#' Allows for the presence of advanced data-generating mechanisms.
#' See Tikka et al. (2021) <doi:10.18637/jss.v099.i05> for further details.
#'
#' # See also
#'
#' * The package vignette.
#' * [dosearch::dosearch()] for instructions and various examples.
#' * <https://github.com/santikka/dosearch/issues/> to submit a bug report.
#'
#' @useDynLib dosearch, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @author Santtu Tikka, Antti Hyttinen, Juha Karvanen
#' @references S. Tikka, A. Hyttinen and J. Karvanen.
#'   "Causal effect identification from multiple incomplete data sources:
#'   a general search-based approach." \emph{Journal of Statistical Software},
#'   99(5):1--40, 2021.
#'
#' @srrstats {G1.3} All terminology is clarified and unambiguously defined.
#' @srrstats {G1.4} `roxygen2` is used for the package documentation.
#' @srrstats {G1.4a} All internal (non-exported) functions are documented.
#' @srrstats {G1.5} Code to reproduce all results of the primary reference
#'   is included in the 'rep' directory of the repository.
#' @srrstats {G2.0, G2.0a} Lengths of inputs are asserted and documented
#' @srrstats {G2.1, G2.1a} Types of inputs are asserted and documented.
#' @srrstats {G2.2} Dimensions of parameters are checked
#' @srrstats {G2.3, G2.3b} All character type parameters are case-sensitive
#'   which is documented.
#' @srrstats {G2.4, G2.4a, G2.4b, G2.4c} Conversions between types are handled
#'   appropriately
#' @srrstats {G2.6} All input is appropriately pre-processed
#' @srrstats {G2.8} Sub-functions receive well-defined arguments
#' @srrstats {G3.0} Floating point numbers are not compared for equality.
#' @srrstats {G5.0} While the package does not use data per se, well known
#'   graphs are used as examples and testing.
#' @srrstats {G5.1} Data set is exported and documented
#'   (`bivariate_missingness`).
#'
#' @srrstats {NW4.0} Only STL is used, with C++11.
#' @srrstats {Nw4.1} It is likely that algorithms for d-separation or
#'   m-separation exist in other C++ libraries, but d-separation is
#'   very simple, and does not warrant the additional dependency.
#' @srrstats {NW4.2} Number of iterations is not well-defined or a meaningful
#'   metric for `dosearch`, but the iteration can be controlled
#'   with a time limit.
#' @srrstats {NW4.3, NW4.3a} Convergence, if loosely understood here as
#'   the search either reaching the target or fails to find it, is guaranteed
#'   because the set of to be applied is finite, and the same distribution is
#'   not derived again. A formal proof for soundness can be found in the
#'   primary reference.
#' @srrstats {NW4.3b} Related to the previous, the are also special criteria to
#'   immediately detect non-convergence from the inputs and terminate early.
#' @srrstats {NW4.4} Related to NW4.2, the time taken by the search can
#'   be returned and even time taken by each individual derivation rule.
#' @srrstats {NW4.5} Loops over matrix dimensions are carried out in C++
#'
"_PACKAGE"

#' Systematic Analysis of Bivariate Missing Data Problems
#'
#' This data set contains the results of a systematic analysis of all missing
#' data problems of two variables. Each problem is associated with a graph
#' containing two vertices, \eqn{X} and \eqn{Y}, and their response indicators,
#' \eqn{R_X} and \eqn{R_Y}.
#'
#' @docType data
#' @keywords datasets
#' @usage data(bivariate_missingness)
#' @source Tikka et al. <https://arxiv.org/abs/1902.01073>
#' @format A data frame with 6144 rows and 8 variables:
#' \describe{
#'   \item{graph}{the graph of the instance.}
#'   \item{nedges}{number of edges in the graph (directed and bidirected).}
#'   \item{arrowXtoY}{whether the graph contains an arrow
#'     from \eqn{X} to \eqn{Y} or not.}
#'   \item{jointXY}{identifiability of the joint distribution
#'     of \eqn{X} and \eqn{Y}}
#'   \item{marginX}{identifiability of the marginal distribution of \eqn{X}.}
#'   \item{marginY}{identifiability of the marginal distribution of \eqn{Y}.}
#'   \item{YcondX}{identifiability of the conditional distribution of \eqn{Y}
#'     given \eqn{X}.}
#'   \item{YdoX}{identifiability of the causal effect of \eqn{X} on \eqn{Y}.}
#' }
"bivariate_missingness"
