\name{get_derivation}
\alias{get_derivation}
\title{Retrieve the derivation of a causal query}
\description{Returns the derivation of causal query of an object of class "\link{dosearch}". }

\usage{get_derivation(x, run_again = FALSE, draw_all = FALSE)}

\arguments{
  \item{x}{an object of class "\link{dosearch}".}
  \item{run_again}{a logical value. If \code{TRUE}, run the search again to obtain a derivation for the query if one was not requested in the function call that produced \code{x}.}
  \item{draw_all}{a logical value. If \code{TRUE}, the derivation will contain every step taken by the search. If \code{FALSE}, only steps that resulted in identification are returned. }
}

\author{Santtu Tikka}

\examples{

data <- "P(x,y,z)"
query <- "P(y|do(x))"
graph <- "
  x -> y
  z -> x
  z -> y
"
x <- dosearch(data, query, graph, control = list(draw_derivation = FALSE))
get_derivation(x, run_again = TRUE)
}