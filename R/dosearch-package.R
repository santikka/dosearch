#' Causal Effect Identification from Multiple Incomplete Data Sources
#'
#' Solves causal effect identifiability problems from arbitrary observational 
#' and experimental data using a heuristic search. 
#' Allows for the presence of advanced data-generating mechanisms. 
#' See Tikka et al. (2021) <doi:10.18637/jss.v099.i05> for further details.
#' 
#' @docType package
#' @name dosearch-package
#' @useDynLib dosearch, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @author Santtu Tikka, Antti Hyttinen, Juha Karvanen
#' @references 
#'
#' S. Tikka, A. Hyttinen and J. Karvanen. 
#' Causal effect identification from multiple incomplete data sources: 
#' a general search-based approach. \emph{Journal of Statistical Software}, 
#' 99(5):1--40, 2021.
#' 
#' @srrstats {G1.0} Primary reference is listed.
#' @srrstats {G1.1} First implementation of an original algorithm.
#' @srrstatsTODO {G1.2} *Statistical Software should include a* Life Cycle Statement *describing current and anticipated future states of development.* 
#' @srrstatsTODO {G1.3} All terminology is clarified and unambiguously defined.
#' @srrstats {G1.4} `roxygen2` is used for the package documentation.
#' @srrstatsTODO {G1.4a} All internal (non-exported) functions are documented.
#' @srrstatsTODO {G1.5} *Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications.* 
#' @srrstatsTODO {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstatsTODO {G2.0a} *Provide explicit secondary documentation of any expectations on lengths of inputs*
#' @srrstatsTODO {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#' @srrstatsTODO {G2.1a} *Provide explicit secondary documentation of expectations on data types of all vector inputs.*
#' @srrstatsTODO {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstatsTODO {G2.3} *For univariate character input:*
#' @srrstatsTODO {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' @srrstatsTODO {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstatsTODO {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstatsTODO {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstatsTODO {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.* 
#' @srrstats {G2.8} Sub-functions receive well-defined arguments
#' @srrstats {G3.0} Floating point numbers are not compared for equality.
#' @srrstats {G5.0} While the package does not use data per se, well known
#'   graphs are used as examples and testing.
#' @srrstats {G5.1} Data set is exported (bivariate_missingness).
NULL

#' Systematic Analysis of Bivariate Missing Data Problems
#'
#' @description
#' This data set contains the results of a systematic analysis of all missing 
#' data problems of two variables. Each problem is associated with a graph 
#' containing two vertices, \eqn{X} and \eqn{Y}, and their response indicators, 
#' \ifelse{html}{\out{R<sub>X</sub>}}{\eqn{R_X}} and 
#' \ifelse{html}{\out{R<sub>Y</sub>}}{\eqn{R_Y}}.
#' 
#' @usage data(bivariate_missingness)
#' @keywords datasets
#' @format A data frame with 6144 rows and 8 variables:
#' @source Tikka et. al. (2021) <arXiv:1902.01073>
#' \describe{
#'   \item{graph}{the graph of the instance.}
#'   \item{nedges}{number of edges in the graph (directed and bidirected).}
#'   \item{arrowXtoY}{whether the graph contains an arrow 
#'     from \eqn{X} to \eqn{Y} or not }
#'   \item{jointXY}{identifiability of the joint distribution 
#'     of \eqn{X} and \eqn{Y} }
#'   \item{marginX}{identifiability of the marginal distribution of \eqn{X} }
#'   \item{marginY}{identifiability of the marginal distribution of \eqn{Y} }
#'   \item{YcondX}{identifiability of the conditional distribution of \eqn{Y} 
#'     given \eqn{X} }
#'   \item{YdoX}{identifiability of the causal effect of \eqn{X} on \eqn{Y} }
#' }
"bivariate_missingness"