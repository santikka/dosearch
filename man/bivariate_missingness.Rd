\name{bivariate_missingness}
\docType{data}
\alias{bivariate_missingness}
\title{Systematic Analysis of Bivariate Missing Data Problems}
\description{
  This data set contains the results of a systematic analysis of all missing data problems of two variables. Each problem is associated with a graph containing two vertices, \eqn{X} and \eqn{Y}, and their response indicators, \ifelse{html}{\out{R<sub>X</sub>}}{\eqn{R_X}} and \ifelse{html}{\out{R<sub>Y</sub>}}{\eqn{R_Y}}.
}
\usage{data(bivariate_missingness)}
\format{A data frame with 6144 rows and 8 variables:
\describe{
    \item{graph}{the graph of the instance, see \code{\link{get_derivation}} for more details on the syntax }
    \item{nedges}{number of edges in the graph (directed and bidirected) }
    \item{arrowXtoY}{whether the graph contains an arrow from \eqn{X} to \eqn{Y} or not }
    \item{jointXY}{identifiability of the joint distribution of \eqn{X} and \eqn{Y} }
    \item{marginX}{identifiability of the marginal distribution of \eqn{X} }
    \item{marginY}{identifiability of the marginal distribution of \eqn{Y} }
    \item{YcondX}{identifiability of the conditional distribution of \eqn{Y} given \eqn{X} }
    \item{YdoX}{identifiability of the causal effect of \eqn{X} on \eqn{Y} }
} % END DESCRIBE
} % END FORMAT
\source{See the Vignette, Section 5.1.}
\keyword{datasets}
