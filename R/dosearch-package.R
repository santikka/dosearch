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